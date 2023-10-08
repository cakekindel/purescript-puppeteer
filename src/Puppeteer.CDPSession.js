import { CDPSession } from 'puppeteer'

/** @type {(_: CDPSession) => () => Promise<void>} */
export const _detach = cdp => () => cdp.detach()
