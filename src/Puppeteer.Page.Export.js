import { Page } from 'puppeteer'

/** @type {(_1: import("puppeteer").PDFOptions) => (_0: Page) => () => Promise<Buffer>} */
export const _pdf = o => p => () => p.pdf(o)

/** @type {(_1: import("puppeteer").ScreenshotOptions) => (_0: Page) => () => Promise<Buffer>} */
export const _screenshot = o => p => () => p.screenshot(o)
