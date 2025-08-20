module Data.Text.ANSI
  ( ANSI(..)
  , ANSIColor
  , ansiB
  , ansiC
  , ansiI
  , strAnsi
  , strAnsiPure
  )
  where

import Prelude
import Data.List (List(..), foldMap, (:))

-------------------------------------------------------------------------------

-- | ANSI text, control, and escape codes.
-- * https://gist.github.com/ConnerWill/d4b6c776b509add763e17f9f113fd25b
data ANSI
  = ANSIPure String
  | ANSIText (List ANSI)
  | ANSIBell
  | ANSIBackSpace
  | ANSIHorzTab
  | ANSILineFeed
  | ANSIVerticalTab
  | ANSIFormFeed
  | ANSICarriageReturn
  | ANSIDelete
  | -- | Limited support.
    ANSIHideCursor
  | -- | Limited support.
    ANSIShowCursor
  | -- | Limited support.
    ANSISaveScreen
  | -- | Limited support.
    ANSIRestoreScreen
  | -- | Limited support.
    ANSIEnableAlt
  | -- | Limited support.
    ANSIDisableAlt
  | ANSIToHome
  | ANSIToPos Int Int
  | ANSIUp Int
  | ANSIDown Int
  | ANSIRight Int
  | ANSILeft Int
  | ANSIToNextLine Int
  | ANSIToPrevLine Int
  | ANSIToCol Int
  | -- | Limited support.
    ANSISavePos
  | -- | Limited support.
    ANSIRestorePos
  | ANSIResetStyles
  | ANSIBold
  | ANSIItalic
  | ANSIUnderline
  | ANSIBlink
  | ANSIReverse
  | ANSIStrikeThrough
  | ANSIForeground ANSIColor
  | ANSIBackground ANSIColor

type ANSIColor = Int

-------------------------------------------------------------------------------

strAnsi :: ANSI -> String
strAnsi (ANSIPure s) = s
strAnsi (ANSIText xs) = foldMap strAnsi xs
strAnsi ANSIHideCursor = "\x1B[?25l"
strAnsi ANSIShowCursor = "\x1B[?25h"
strAnsi ANSISaveScreen = "\x1B[?47h"
strAnsi ANSIRestoreScreen = "\x1B[?47l"
strAnsi ANSIEnableAlt = "\x1B[?1049h"
strAnsi ANSIDisableAlt = "\x1B[?1049l"
strAnsi ANSISavePos = "\x1B 7"
strAnsi ANSIRestorePos = "\x1B 8"
strAnsi ANSIResetStyles = "\x1B[0m"
strAnsi ANSIBold = "\x1B[1m"
strAnsi ANSIItalic = "\x1B[3m"
strAnsi ANSIUnderline = "\x1B[4m"
strAnsi ANSIBlink = "\x1B[5m"
strAnsi ANSIReverse = "\x1B[7m"
strAnsi ANSIStrikeThrough = "\x1B[9m"
strAnsi (ANSIForeground x) = "\x1B[38;5;" <> show x <> "m"
strAnsi (ANSIBackground x) = "\x1B[48;5;" <> show x <> "m"
strAnsi _ = "\\todo\\"


strAnsiPure :: ANSI -> String
strAnsiPure (ANSIPure s) = s
strAnsiPure (ANSIText xs) = foldMap strAnsi xs
strAnsiPure _ = ""

-------------------------------------------------------------------------------

ansiC :: ANSIColor -> String -> ANSI
ansiC x s = ANSIForeground x <> ANSIPure s <> ANSIResetStyles

ansiB :: ANSIColor -> String -> ANSI
ansiB x s = ANSIBackground x <> ANSIPure s <> ANSIResetStyles

ansiI :: String -> ANSI
ansiI s = ANSIItalic <> ANSIPure s <> ANSIResetStyles

-------------------------------------------------------------------------------

instance semigroupAnsi :: Semigroup ANSI where
  append (ANSIText as) (ANSIText bs) = ANSIText $ as <> bs
  append (ANSIText as) (b) = ANSIText $ as <> (b:Nil)
  append (a) (ANSIText bs) = ANSIText $ a : bs
  append (a) (b) = ANSIText (a:b:Nil)

instance Monoid ANSI where
  mempty :: ANSI
  mempty = ANSIText Nil

-------------------------------------------------------------------------------
