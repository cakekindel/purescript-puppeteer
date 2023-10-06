import { Page, JSHandle } from 'puppeteer'

/**
 * @type {(_: string) => (_: Page | JSHandle<unknown>) => (_: Array<unknown>) => () => Promise<unknown>}
 */
export const _run = s => h => a => () => {
  /** @type {any} */
  const f = new Function(`return (${s})(...arguments)`)
  /** @type {(_s: () => void, ...as: Array<unknown>) => Promise<unknown>} */
  const ev = h.evaluate.bind(h)
  return ev(f, ...a)
}

/**
 * @type {(_: string) => (_: Page | JSHandle<unknown>) => (_: Array<unknown>) => () => Promise<JSHandle<unknown>>}
 */
export const _runh = s => h => a => () => {
  /** @type {any} */
  const f = new Function(`return (${s})(...arguments)`)
  /** @type {(_s: () => void, ...as: Array<unknown>) => Promise<JSHandle<unknown>>} */
  const ev = h.evaluateHandle.bind(h)
  return ev(f, ...a)
}
