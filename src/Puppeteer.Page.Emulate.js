import { Page } from 'puppeteer'
import { KnownDevices } from 'puppeteer'
import Cdp from 'devtools-protocol'

/** @type {(_0: keyof KnownDevices) => import('puppeteer').Device} */
export const _knownDevice = k => KnownDevices[k]

/** @type {(_0: import('puppeteer').Device) => (_1: Page) => Promise<void>} */
export const _emulate = d => p => p.emulate(d)

/** @type {(_0: number) => (_1: Page) => Promise<void>} */
export const _cpuThrottling = f => p => p.emulateCPUThrottling(f)

/** @type {(_0: { isUserActive: boolean, isScreenUnlocked: boolean } | undefined) => (_1: Page) => Promise<void>} */
export const _idle = o => p => p.emulateIdleState(o)

/** @type {(_1: Page) => Promise<void>} */
export const _emulatePrint = p => p.emulateMediaType('print')

/** @type {(_1: Page) => Promise<void>} */
export const _unemulatePrint = p => p.emulateMediaType(undefined)

/** @type {(_0: import('puppeteer').NetworkConditions) => (_1: Page) => Promise<void>} */
export const _emulateNetwork = c => p => p.emulateNetworkConditions(c)

/** @type {(_1: Page) => Promise<void>} */
export const _unemulateNetwork = p => p.emulateNetworkConditions(null)

/** @type {(_0: string) => (_1: Page) => Promise<void>} */
export const _emulateTimezone = c => p => p.emulateTimezone(c)

/** @type {(_1: Page) => Promise<void>} */
export const _unemulateTimezone = p => p.emulateTimezone(undefined)

/** @type {(_0: import('devtools-protocol').Protocol.Emulation.SetEmulatedVisionDeficiencyRequest['type']) => (_1: Page) => Promise<void>} */
export const _emulateVisionDeficiency = c => p => p.emulateVisionDeficiency(c)

/** @type {(_1: Page) => Promise<void>} */
export const _unemulateVisionDeficiency = p =>
  p.emulateVisionDeficiency(undefined)

/** @type {(_0: import('puppeteer').GeolocationOptions) => (_1: Page) => Promise<void>} */
export const _setGeolocation = g => p => p.setGeolocation(g)
/** @type {(_0: Page) => Promise<void>} */
export const _unsetGeolocation = p =>
  p
    .createCDPSession()
    .then(cdp => cdp.send('Emulation.clearGeolocationOverride'))

/** @type {(_0: Page) => Promise<void>} */
export const _setJavascriptDisabled = p => p.setJavaScriptEnabled(false)
/** @type {(_0: Page) => Promise<void>} */
export const _unsetJavascriptDisabled = p => p.setJavaScriptEnabled(true)

/** @type {(_0: Page) => Promise<void>} */
export const _setOffline = p => p.setOfflineMode(true)
/** @type {(_0: Page) => Promise<void>} */
export const _unsetOffline = p => p.setOfflineMode(false)

/** @type {(_0: String) => (_1: Page) => Promise<void>} */
export const _setUserAgent = ua => p => p.setUserAgent(ua)
