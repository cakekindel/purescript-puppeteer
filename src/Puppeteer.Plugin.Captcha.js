import { PuppeteerExtra } from 'puppeteer-extra'
import Captcha from 'puppeteer-extra-plugin-recaptcha'

/** @typedef {import('puppeteer-extra-plugin-recaptcha/dist/types').PluginOptions} PluginOptions */
/** @typedef {import('puppeteer-extra-plugin-recaptcha/dist/types').CaptchaInfo} CaptchaInfo */
/** @typedef {import('puppeteer-extra-plugin-recaptcha/dist/types').CaptchaSolution} CaptchaSolution */
/** @typedef {import('puppeteer-extra-plugin-recaptcha/dist/types').CaptchaSolved} CaptchaSolved */
/** @typedef {import('puppeteer').Page & import('puppeteer-extra-plugin-recaptcha/dist/types').RecaptchaPluginPageAdditions} Page */

/** @type {(_: PluginOptions) => (_: PuppeteerExtra) => () => PuppeteerExtra} */
export const _captcha = o => p => () => p.use(Captcha(o))

/** @type {(_: Page) => Promise<{captchas: CaptchaInfo[], filtered: unknown[]}>} */
export const _findCaptchas = p => p.findRecaptchas()

/** @type {(_: Page) => (_: CaptchaInfo[]) => Promise<{solutions: CaptchaSolution[]}>} */
export const _getSolutions = p => cs => p.getRecaptchaSolutions(cs)

/** @type {(_: Page) => (_: CaptchaSolution[]) => Promise<{solved: CaptchaSolved[]}>} */
export const _enterSolutions = p => cs => p.enterRecaptchaSolutions(cs)

/** @type {(_: Page) => Promise<{captchas: CaptchaInfo[], filtered: unknown[], solutions: CaptchaSolution[], solved: CaptchaSolved[]}>} */
export const _solveCaptchas = p => p.solveRecaptchas()
