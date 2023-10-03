import { PuppeteerExtra } from 'puppeteer-extra'
import Stealth from 'puppeteer-extra-plugin-stealth'

/** @type {(_: PuppeteerExtra) => () => PuppeteerExtra} */
export const install = p => () => p.use(Stealth())
