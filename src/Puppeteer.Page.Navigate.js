import { Page } from 'puppeteer'

/** @type {(ev: import('puppeteer').PuppeteerLifeCycleEvent) => (p: Page) => Promise<any>} */
export const _forward = ev => p => p.goForward({ timeout: 0, waitUntil: ev })

/** @type {(ev: import('puppeteer').PuppeteerLifeCycleEvent) => (p: Page) => Promise<any>} */
export const _back = ev => p => p.goBack({ timeout: 0, waitUntil: ev })

/** @type {(url: string) => (ev: import('puppeteer').PuppeteerLifeCycleEvent) => (p: Page) => Promise<any>} */
export const _to = url => ev => p => p.goto(url, { timeout: 0, waitUntil: ev })

/** @type {(ev: import('puppeteer').PuppeteerLifeCycleEvent) => (p: Page) => Promise<any>} */
export const _reload = ev => p => p.goForward({ timeout: 0, waitUntil: ev })
