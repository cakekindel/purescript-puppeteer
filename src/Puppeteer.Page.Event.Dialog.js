// foreign import defaultValue :: Dialog -> String
// foreign import message :: Dialog -> String
// foreign import _dismiss :: Dialog -> Promise Unit
// foreign import _accept :: Foreign -> Dialog -> Promise Unit
// foreign import _type :: Dialog -> String

import { Dialog } from 'puppeteer'

/** @type {(d: Dialog) => string} */
export const defaultValue = d => d.defaultValue()

/** @type {(d: Dialog) => string} */
export const message = d => d.message()

/** @type {(d: Dialog) => Promise<void>} */
export const _dismiss = d => d.dismiss()

/** @type {(s: string | undefined) => (d: Dialog) => Promise<void>} */
export const _accept = s => d => d.accept(s)

/** @type {(d: Dialog) => string} */
export const _type = d => d.type()
