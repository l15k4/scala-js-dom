package org.scalajs.dom.extensions

import org.scalajs.dom
import org.scalajs.dom.{Element, Event, KeyboardEvent}

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.UndefOr

sealed trait KeyDownBuffer {
  val keyDownList: ListBuffer[KeyboardEvent]
}

sealed trait DownHandler extends KeyDownBuffer with PressHandler {

  /**
   * Returns true if the key fires a keypress event in the current browser.
   */
  private def firesKeyPressEvent(event: KeyboardEvent): Boolean = {
    if (!Device.isIE && !Device.isWebKit) {
      return true
    }

    if (Device.isMac && event.altKey) {
      return KCode.isUsKeyboardCharKey(event.keyCode)
    }

    // Alt but not AltGr which is represented as Alt+Ctrl.
    if (event.altKey && !event.ctrlKey) {
      return false
    }

    // Saves Ctrl or Alt + key for IE and WebKit, which won't fire keypress.
    if (!event.shiftKey && (keyDownList.last.keyCode == KCode.Ctrl || keyDownList.last.keyCode == KCode.Alt || Device.isMac && keyDownList.last.keyCode == KCode.Meta)) {
      return false
    }

    // Some keys with Ctrl/Shift do not issue keypress in WebKit.
    if (Device.isWebKit && event.ctrlKey && event.shiftKey && Map(
        KCode.backslash,
        KCode.squareBracketOpen,
        KCode.squareBracketClose,
        KCode.graveAccent,
        KCode.semicolon,
        KCode.dash,
        KCode.equals,
        KCode.comma,
        KCode.period,
        KCode.slash,
        KCode.singleQuote
      ).contains(event.keyCode)
    ) {
      return false
    }

    event.keyCode match {
      case KCode.Enter => !Device.isIE // IE9 does not fire keypress on ENTER.
      case KCode.Escape => !Device.isWebKit
      case other => KCode.isUsKeyboardCharKey(event.keyCode)
    }

  }

  /** Handle keydown events. */
  def processKeyDown(e: KeyboardEvent): KeyboardEvent = {
    import KeyboardEventHandler._

    val holdenKeyDown = keyDownList.lastOption.map(_.keyCode)
    // Ctrl-Tab and Alt-Tab breaks our keyDownList with ctrl/alt/meta abandoned in it
    holdenKeyDown.foreach { last =>
      if ((last == KCode.Ctrl && !e.ctrlKey) || (last == KCode.Alt && !e.altKey) || (Device.isMac && last == KCode.Meta && !e.metaKey))
        keyDownList.clear()
    }

    e.normalizeKeyCode()

    // Technically a "keydown" event doesn't have a charKeyCode. This is calculated nonetheless to provide us with more information
    // in giving as much information as possible on keypress about keycode and also charKeyCode.
    e.resolveCharCode()

    // holding a letter produces not only xxx keypresses but xxx keydowns too, we don't want to enqueue them
    if (holdenKeyDown.isEmpty || (holdenKeyDown.isDefined && holdenKeyDown.get == e.keyCode)) {
      println(s"enqueing ${e.`type`} ch: ${e.shadowCharCode} k: ${e.shadowKeyCode} to $keyDownList")
      keyDownList += e
    }
    if (keyDownList.nonEmpty && e.keyCode != keyDownList.last.keyCode && !firesKeyPressEvent(e))
      dom.console.warn(s"Event $e probably wont be followed by onpress event even though it should be")
    e
  }
}

sealed trait PressHandler extends KeyDownBuffer {

  /**
   * Given the previously recorded keydown key codes, see if we can determine the keycode of this keypress [event].
   * (Generally browsers only provide charCode information for keypress events, but with a little reverse-engineering, we can also determine the keyKeyCode.)
   * @note This function is a work in progress. We'll expand this function once we get more information about other keyboards.
   */
  private def resolveKeyCode(event: KeyboardEvent): Option[Int] = {
    import KeyboardEventHandler._
    def isShift = event.shiftKey && KCode.isUpperCaseLetter(event.charCode)
    keyDownList.lastOption.collect {
      case prevEvent if prevEvent.shadowCharCode.fold(false)(_ == event.charCode) => prevEvent.keyCode
      case prevEvent if prevEvent.shadowCharCode.fold(false)(_ == event.charCode + KCode.letterSizeOffset) && isShift => prevEvent.keyCode
    }
  }

  /** Handle keypress events. */
  def processKeyPress(e: KeyboardEvent): KeyboardEvent = {
    import KeyboardEventHandler._

    // IE reports the character code in the keyCode field for keypress events. There are two exceptions however, Enter and Escape.
    if (Device.isIE) {
      if (e.keyCode != KCode.Enter && e.keyCode != KCode.Escape) {
        e.shadowCharCode(e.keyCode)
      }
    } else if (Device.isOpera) {
      // Opera reports the character code in the keyCode field.
      if (KCode.isUsKeyboardCharKey(e.keyCode)) e.shadowCharCode(e.keyCode)
    }
    // Now we estimate what the keycode is that was actually pressed, given previous keydown information.
    resolveKeyCode(e).foreach(e.shadowKeyCode(_))

    e.shadowCharCode(e.charCode)

    // Correct the key value for certain browser-specific quirks.
    e.keyIdentifier.foreach { ki =>
      // This is needed for Safari Windows because it currently doesn't give a keyCode/which for non printable keys.
      KCode.correctionKeyIdentifiers.lift(ki).foreach(e.shadowKeyCode(_))
    }

    e.shadowAltKey(keyDownList.forall(element => element.altKey))
    dom.console.log(s"onpress: ${e.`type`} ch: ${e.shadowCharCode} k: ${e.shadowKeyCode}")
    e
  }

}

sealed trait UpHandler extends KeyDownBuffer {
  import KeyboardEventHandler._
  /** Handle keyup events. */
  def processKeyUp(e: KeyboardEvent): KeyboardEvent = {
    val toRemove = keyDownList.filter(key => key.keyCode == e.keyCode).lastOption
    if (toRemove.isDefined) {
      val r = toRemove.get
      r.shadowCharCode.foreach(e.shadowCharCode(_))
      r.shadowKeyCode.foreach(e.shadowKeyCode(_))
      r.shadowAltKey.foreach(e.shadowAltKey(_))
      dom.console.log(s"dequeing ${e.`type`} ch: ${e.shadowCharCode} k: ${e.shadowKeyCode} from $keyDownList")
      keyDownList.remove(keyDownList.indexOf(r))
    } else if (keyDownList.nonEmpty) {
      // This happens when we've reached some international keyboard case we haven't accounted for or we
      // haven't correctly eliminated all browser inconsistencies. Filing bugs on when this is reached is welcome!
      dom.console.log(s"Failed dequeing ${e.`type`} ch: ${e.shadowCharCode} k: ${e.shadowKeyCode} from $keyDownList")
      keyDownList.remove(keyDownList.size - 1)
    }
    e
  }
}

class KeyboardEventHandler extends DownHandler with UpHandler {
  val keyDownList = ListBuffer[KeyboardEvent]()
}

object KeyboardEventHandler {

  def forAllEvents(target: Element, down: KeyboardEvent => _, press: KeyboardEvent => _, up: KeyboardEvent => _) = {
    polyfillEvents(target, down, press, up)
  }
  
  /**
  * @note We need to listen to events of all types internally to be able to polyfill any of them, even though we are interested in only one type
  * @see keyDownList
  */
  def forDownEvents(target: Element, fn: KeyboardEvent => _) = {
    polyfillEvents(target, down = fn)
  }

  /**
  * @note We need to listen to events of all types internally to be able to polyfill any of them, even though we are interested in only one type
  * @see keyDownList
  */
  def forPressEvents(target: Element, fn: KeyboardEvent => _) = {
    polyfillEvents(target, press = fn)
  }

  /**
  * @note We need to listen to events of all types internally to be able to polyfill any of them, even though we are interested in only one type
  * @see keyDownList
  */
  def forUpEvents(target: Element, fn: KeyboardEvent => _) = {
    polyfillEvents(target, up = fn)
  }

  private def polyfillEvents(target: Element, down: KeyboardEvent => _ = null, press: KeyboardEvent => _ = null, up: KeyboardEvent => _ =  null) = {
    val handler = new KeyboardEventHandler
    target.addEventListener(
      "keydown",
      (e: Event) => {
        val polyfilledEvent = handler.processKeyDown(e.asInstanceOf[KeyboardEvent])
        Option(down).foreach(fn => fn(polyfilledEvent))
      },
      useCapture = true
    )
    target.addEventListener(
      "keypress",
      (e: Event) => {
        val polyfilledEvent = handler.processKeyPress(e.asInstanceOf[KeyboardEvent])
        Option(press).foreach(fn => fn(polyfilledEvent))
      },
      useCapture = true
    )
    target.addEventListener(
      "keyup",
      (e: Event) => {
        val polyfilledEvent = handler.processKeyUp(e.asInstanceOf[KeyboardEvent])
        Option(up).foreach(fn => fn(polyfilledEvent))
      },
      useCapture = true
    )
  }

  implicit class ShadowKeyEvent(e: KeyboardEvent) {

    private [this] def setDynamic(name: String, value: js.Any) = e.asInstanceOf[js.Dynamic].updateDynamic(name)(value)
    private [this] def getDynamic[T](name: String): Option[T] = Option((e.asInstanceOf[js.Dynamic].selectDynamic(name): UndefOr[Dynamic]).orNull).asInstanceOf[Option[T]]

    def normalizeKeyCode() {
      shadowKeyCode(
        if (Device.isGecko)
          e.keyCode match {
            case KCode.equalsGecko._1 => KCode.equals._1
            case KCode.semicolonGecko._1 => KCode.semicolon._1
            case 224 => KCode.Meta
            case 0 => KCode.Win
            case other => e.keyCode
          }
        else
          e.keyCode
      )
    }

    def resolveCharCode() {
      require(e.`type` == "keydown")
      val keyCode = e.keyCode
      val charCode =
        if (e.location == 3) {
          KCode.numpadKey2Char.lift(keyCode)
        } else if (KCode.isLetter(keyCode)) {
          // Set the "char code" for key down as the lower case letter. Again, this
          // will not show up for the user, but will be helpful in estimating
          // keyCode locations and other information during the keyPress event.
          Option(KCode.letterKey2Char(keyCode))
        } else {
          KCode.shiftableKey2Char.lift(keyCode) match {
            case Some(fn) => Option(fn(e.shiftKey))
            case None =>
              keyCode match {
                // this place is open for additional keys
                case KCode.Enter => Option(KCode.Enter)
                case KCode.Space => Option(KCode.Space)
                case toBeDone => None
              }
          }
        }
      charCode.foreach(shadowCharCode(_))
    }

    def shadowKeyCode(keyCode: Int) = setDynamic("shadowKeyCode", keyCode)
    def shadowCharCode(charCode: Int) = setDynamic("shadowCharCode", charCode)
    def shadowAltKey(altKey: Boolean) = setDynamic("shadowAltKey", altKey)

    def shadowKeyCode: Option[Int] = getDynamic("shadowKeyCode")
    def shadowCharCode: Option[Int] = getDynamic("shadowCharCode")
    def shadowAltKey: Option[Boolean] = getDynamic("shadowAltKey")

    // Google Chrome and Safari support KeyboardEvent.keyIdentifier which is defined in the old draft of DOM Level 3 Events
    def keyIdentifier: Option[String] = getDynamic("keyIdentifier")
  }
}


object KCode {

  /** numbers and Upper case letters have CharCode equal to KeyCode */

  val Num0 = '0'.toInt // 48
  val Num1 = '1'.toInt // 49
  val Num2 = '2'.toInt // 50
  val Num3 = '3'.toInt // 51
  val Num4 = '4'.toInt // 52
  val Num5 = '5'.toInt // 53
  val Num6 = '6'.toInt // 54
  val Num7 = '7'.toInt // 55
  val Num8 = '8'.toInt // 56
  val Num9 = '9'.toInt // 57

  def isNumber(keyCode: Int) = keyCode >= Num0 && keyCode <= Num9

  /** [A-Z] charCode is equal to [a-z] keyCode, thus I won't duplicate constants */

  val letterSizeOffset = 'a'.toInt - 'A'.toInt // just use this offset if you need to work with lower/upper case letters

  def isLetter(keyCode: Int) = keyCode >= a && keyCode <= z
  def isUpperCaseLetter(charCode: Int) = isLetter(charCode)
  def letterKey2Char(keyCode: Int) = keyCode + letterSizeOffset

  val a = 'A'.toInt // 65
  val b = 'B'.toInt // 66
  val c = 'C'.toInt // 67
  val d = 'D'.toInt // 68
  val e = 'E'.toInt // 69
  val f = 'F'.toInt // 70
  val g = 'G'.toInt // 71
  val h = 'H'.toInt // 72
  val i = 'I'.toInt // 73
  val j = 'J'.toInt // 74
  val k = 'K'.toInt // 75
  val l = 'L'.toInt // 76
  val m = 'M'.toInt // 77
  val n = 'N'.toInt // 78
  val o = 'O'.toInt // 79
  val p = 'P'.toInt // 80
  val q = 'Q'.toInt // 81
  val r = 'R'.toInt // 82
  val s = 'S'.toInt // 83
  val t = 'T'.toInt // 84
  val u = 'U'.toInt // 85
  val v = 'V'.toInt // 86
  val w = 'W'.toInt // 87
  val x = 'X'.toInt // 88
  val y = 'Y'.toInt // 89
  val z = 'Z'.toInt // 90

  /** numpad numbers share common numbers charCode */

  val Numpad0 = (96, Num0)
  val Numpad1 = (97, Num1)
  val Numpad2 = (98, Num2)
  val Numpad3 = (99, Num3)
  val Numpad4 = (100, Num4)
  val Numpad5 = (101, Num5)
  val Numpad6 = (102, Num6)
  val Numpad7 = (103, Num7)
  val Numpad8 = (104, Num8)
  val Numpad9 = (105, Num9)
  val NumpadMultiply =  (106, '*'.toInt)
  val NumpadAdd =       (107, '+'.toInt)
  val NumpadSubtract =  (109, '-'.toInt)
  val NumpadDivide =    (111, '/'.toInt)
  val NumpadPeriod =    (110, '.'.toInt)

  val numpadKey2Char = Map(
    Numpad0,
    Numpad1,
    Numpad2,
    Numpad3,
    Numpad4,
    Numpad5,
    Numpad6,
    Numpad7,
    Numpad8,
    Numpad9,
    NumpadMultiply,
    NumpadAdd,
    NumpadSubtract,
    NumpadDivide,
    NumpadPeriod
  )

  def isNumpad(keyCode: Int) = numpadKey2Char.contains(keyCode)

  /** keys that have different charCode representation when shift key is pressed */

  val comma =                 (188, (shift: Boolean) => if (shift) '<'.toInt else ','.toInt)
  val dash =                  (189, (shift: Boolean) => if (shift) '_'.toInt else '-'.toInt)
  val period =                (190, (shift: Boolean) => if (shift) '>'.toInt else '.'.toInt)
  val slash =                 (191, (shift: Boolean) => if (shift) '?'.toInt else '/'.toInt)
  val graveAccent =           (192, (shift: Boolean) => if (shift) '~'.toInt else '`'.toInt)
  val squareBracketOpen =     (219, (shift: Boolean) => if (shift) '{'.toInt else '['.toInt)
  val backslash =             (220, (shift: Boolean) => if (shift) '|'.toInt else '\\'.toInt)
  val squareBracketClose =    (221, (shift: Boolean) => if (shift) '}'.toInt else ']'.toInt)
  val singleQuote =           (222, (shift: Boolean) => if (shift) '"'.toInt else '''.toInt)

  // gecko has semicolon and equals keyCode equal to charCode
  val semicolon =             (186, (shift: Boolean) => if (shift) ':'.toInt else ';'.toInt)
  val semicolonGecko =  (';'.toInt, (shift: Boolean) => if (shift) ':'.toInt else ';'.toInt)
  val equals =                (187, (shift: Boolean) => if (shift) '+'.toInt else '='.toInt)
  val equalsGecko =     ('='.toInt, (shift: Boolean) => if (shift) '+'.toInt else '='.toInt)

  val num0 = (Num0, (shift: Boolean) => if (shift) ')'.toInt else Num0)
  val num1 = (Num1, (shift: Boolean) => if (shift) '!'.toInt else Num1)
  val num2 = (Num2, (shift: Boolean) => if (shift) '@'.toInt else Num2)
  val num3 = (Num3, (shift: Boolean) => if (shift) '#'.toInt else Num3)
  val num4 = (Num4, (shift: Boolean) => if (shift) '$'.toInt else Num4)
  val num5 = (Num5, (shift: Boolean) => if (shift) '%'.toInt else Num5)
  val num6 = (Num6, (shift: Boolean) => if (shift) '^'.toInt else Num6)
  val num7 = (Num7, (shift: Boolean) => if (shift) '&'.toInt else Num7)
  val num8 = (Num8, (shift: Boolean) => if (shift) '*'.toInt else Num8)
  val num9 = (Num9, (shift: Boolean) => if (shift) '('.toInt else Num9)

  val shiftableKey2Char = Map[Int, Boolean => Int](
    num0,
    num1,
    num2,
    num3,
    num4,
    num5,
    num6,
    num7,
    num8,
    num9,
    comma,
    dash,
    period,
    slash,
    graveAccent,
    squareBracketOpen,
    backslash,
    squareBracketClose,
    singleQuote,
    semicolon,
    semicolonGecko,
    equals,
    equalsGecko
  )

  val Enter = 13
  val Space = 32

  /** Keys that do not have unicode representation */

  val Backspace = 8
  val Tab = 9
  val Shift = 16
  val Ctrl = 17
  val Alt = 18
  val Pause = 19
  val CapsLock = 20
  val Escape = 27
  val PageUp = 33
  val PageDown = 34
  val End = 35
  val Home = 36
  val Left = 37
  val Up = 38
  val Right = 39
  val Down = 40
  val Insert = 45
  val Delete = 46
  val Meta = 91
  val Win = 224

  val F1 = 112
  val F2 = 113
  val F3 = 114
  val F4 = 115
  val F5 = 116
  val F6 = 117
  val F7 = 118
  val F8 = 119
  val F9 = 120
  val F10 = 121
  val F11 = 122
  val F12 = 123

  /** keyCodes for these identifiers might differ in various browsers */
  val correctionKeyIdentifiers = Map[String,Int](
    "Up" -> Up,
    "Down" -> Down,
    "Left" -> Left,
    "Right" -> Right,
    "Enter" -> Enter,
    "F1" -> F1,
    "F2" -> F2,
    "F3" -> F3,
    "F4" -> F4,
    "F5" -> F5,
    "F6" -> F6,
    "F7" -> F7,
    "F8" -> F8,
    "F9" -> F9,
    "F10" -> F10,
    "F11" -> F11,
    "F12" -> F12,
    "U+007F" -> Delete,
    "Home" -> Home,
    "End" -> End,
    "PageUp" -> PageUp,
    "PageDown" -> PageDown,
    "Insert" -> Insert
  )

  /** This is adopted from Dart lang, I'm not quite sure what is the point of it */
  def isUsKeyboardCharKey(keyCode: Int): Boolean = {
    if (isNumber(keyCode) ||  isNumpad(keyCode) || isLetter(keyCode) ) return true
    // Safari sends zero key code for non-latin characters.
    if (Device.isWebKit && keyCode == 0) return true
    shiftableKey2Char.contains(keyCode) || Space == keyCode
  }
}

object Device {

  val IphoneRegex = "iPhone|iPod".r
  val MobileSafariRegex = "iPhone|iPod|iPad".r
  val AppleSimulatorRegex = "iP.*Simulator".r

  lazy val userAgent = dom.window.navigator.userAgent

  lazy val isMac = userAgent.contains("Mac")
  lazy val isIPhone = IphoneRegex.findFirstIn(userAgent).isDefined
  lazy val isIPad = userAgent.contains("iPad")
  lazy val isGecko = userAgent.contains("Gecko")
  lazy val isOpera = userAgent.contains("Opera")
  lazy val isIE = userAgent.contains("Trident/")
  lazy val isWebKit = userAgent.contains("WebKit")
  lazy val isChrome = userAgent.contains("Chrome")
  lazy val isMobileSafari = MobileSafariRegex.findFirstIn(userAgent).isDefined
  lazy val isAppleSimulator = AppleSimulatorRegex.findFirstIn(userAgent).isDefined
  lazy val isAndroid = userAgent.contains("Android")
  lazy val isWebOs = userAgent.contains("webOS")

  lazy val supportsTouch = isMobileSafari || isAndroid
}