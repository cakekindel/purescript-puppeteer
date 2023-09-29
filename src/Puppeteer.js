import { Browser } from 'puppeteer'

/** @type {() => Promise<import('puppeteer-extra').PuppeteerExtra>} */
export const _puppeteer = () => import('puppeteer-extra').then(m => m.default)

/** @type {(o: import('puppeteer').ConnectOptions) => (_2: import('puppeteer').PuppeteerNode) => () => Promise<Browser>} */
export const _connect = o => p => () => p.connect(o)

/** @type {(o: import('puppeteer').LaunchOptions) => (_2: import('puppeteer').PuppeteerNode) => () => Promise<Browser>} */
export const _launch = o => p => () => p.launch(o)
