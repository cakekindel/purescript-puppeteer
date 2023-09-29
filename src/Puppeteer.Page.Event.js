import { Page } from 'puppeteer'

/** @type {(_0: import('puppeteer').PageEvent) => (_1: (_: any) => void) => (_2: Page) => () => [import('puppeteer').PageEvent, (_: any) => void]} */
export const _addListener = t => f => p => () => {
  p.on(t, f)
  return [t, f]
}

/** @type {(_0: [import('puppeteer').PageEvent, (_: any) => void]) => (_2: Page) => () => void} */
export const _removeListener =
  ([t, f]) =>
  p =>
  () =>
    p.off(t, f)

/** @type {(_0: import('puppeteer').PageEvent) => (_1: (_: any) => void) => (_2: Page) => () => void} */
export const _once = t => f => p => () => p.once(t, f)
