import AdBlock, {
  PuppeteerExtraPluginAdblocker,
} from 'puppeteer-extra-plugin-adblocker'
import { PuppeteerExtra } from 'puppeteer-extra'

/** @type {(_: import('puppeteer-extra-plugin-adblocker').PluginOptions) => (_: PuppeteerExtra) => () => PuppeteerExtra} */
export const _install = o => p => () => p.use(AdBlock(o))

/** @type {(_: PuppeteerExtra) => () => Promise<import('@cliqz/adblocker-puppeteer').PuppeteerBlocker>} */
export const _blocker = p => () => {
  const adblock = p.plugins.find(
    pl => pl instanceof PuppeteerExtraPluginAdblocker,
  )

  if (!adblock || !(adblock instanceof PuppeteerExtraPluginAdblocker)) {
    throw new Error('Adblock plugin not registered')
  } else {
    return adblock.getBlocker()
  }
}
