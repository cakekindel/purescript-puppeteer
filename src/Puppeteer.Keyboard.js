import { Keyboard } from 'puppeteer'

/**
 * `foreign import _up :: Foreign -> Keyboard -> Promise Unit`
 * @type {(_1: import("puppeteer").KeyInput) => (_2: Keyboard) => Promise<void>}
 */
export const _up = k => kb => kb.up(k)

/**
 * `foreign import _down :: Foreign -> Keyboard -> Promise Unit`
 * @type {(_1: import("puppeteer").KeyInput) => (_2: Keyboard) => Promise<void>}
 */
export const _down = k => kb => kb.down(k)

/**
 * foreign import _press :: Foreign -> Nullable Int -> Keyboard -> Promise Unit
 * @type {(_1: import('puppeteer').KeyInput) => (_2: number | null) => (_3: Keyboard) => Promise<void>}
 */
export const _press = k => ms => kb => kb.press(k, { delay: ms || undefined })

/**
 * foreign import _type :: String -> Nullable Int -> Keyboard -> Promise Unit
 * @type {(_1: string) => (_2: number | null) => (_3: Keyboard) => Promise<void>}
 */
export const _type = k => ms => kb => kb.type(k, { delay: ms || undefined })
