import { Page } from 'puppeteer'

/** @typedef {import('puppeteer').EventsWithWildcard<import('puppeteer').PageEvents>} Events */

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

/** @type {(_2: Page) => () => void} */
export const removeAllListeners = p => () => p.removeAllListeners()

/** @type {(_2: Page) => () => import('puppeteer').EmitterState<Events>} */
export const eject = p => () => p.eject()

/** @type {(_1: import('puppeteer').EmitterState<Events>) => (_2: Page) => () => void} */
export const inject = s => p => () => {
  p.inject(s)
}
