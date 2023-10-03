import AnonUA from 'puppeteer-extra-plugin-anonymize-ua'
import { PuppeteerExtra } from 'puppeteer-extra'

/** @type {(_: PuppeteerExtra) => () => PuppeteerExtra} */
export const install = p => () => p.use(AnonUA())
