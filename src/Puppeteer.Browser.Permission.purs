module Puppeteer.Browser.Permission (Permission(..), preparePermission) where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Foreign (Foreign)
import Simple.JSON (writeImpl)

data Permission
  = Geolocation
  | Midi
  | Notifications
  | Camera
  | Microphone
  | BackgroundSync
  | AmbientLightSensor
  | Accelerometer
  | Gyroscope
  | Magnetometer
  | AccessibilityEvents
  | ClipboardRead
  | ClipboardWrite
  | ClipboardSanitizedWrite
  | PaymentHandler
  | PersistentStorage
  | IdleDetection
  | MidiSysex

derive instance genericPermission :: Generic Permission _
instance eqPermission :: Eq Permission where
  eq = genericEq

instance ordPermission :: Ord Permission where
  compare = genericCompare

preparePermission :: Permission -> Foreign
preparePermission = writeImpl <<< preparePermission_

preparePermission_ :: Permission -> String
preparePermission_ Geolocation = "geolocation"
preparePermission_ Midi = "midi"
preparePermission_ Notifications = "notifications"
preparePermission_ Camera = "camera"
preparePermission_ Microphone = "microphone"
preparePermission_ BackgroundSync = "background-sync"
preparePermission_ AmbientLightSensor = "ambient-light-sensor"
preparePermission_ Accelerometer = "accelerometer"
preparePermission_ Gyroscope = "gyroscope"
preparePermission_ Magnetometer = "magnetometer"
preparePermission_ AccessibilityEvents = "accessibility-events"
preparePermission_ ClipboardRead = "clipboard-read"
preparePermission_ ClipboardWrite = "clipboard-write"
preparePermission_ ClipboardSanitizedWrite = "clipboard-sanitized-write"
preparePermission_ PaymentHandler = "payment-handler"
preparePermission_ PersistentStorage = "persistent-storage"
preparePermission_ IdleDetection = "idle-detection"
preparePermission_ MidiSysex = "midi-sysex"
