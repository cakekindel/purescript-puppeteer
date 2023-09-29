/// <reference lib="dom" />

import { ElementHandle } from 'puppeteer'
import { JSHandle } from 'puppeteer'

/** @type {<T>(_: { remoteObject: (_0: string) => T, primitive: (_0: unknown) => T }) => (_: JSHandle<unknown>) => () => T} */
export const _id =
  ({ remoteObject, primitive }) =>
  h =>
  () => {
    const oid = h.remoteObject().objectId
    if (oid) {
      return remoteObject(oid)
    } else {
      return primitive(h.remoteObject().value)
    }
  }

/** @type {(_: string) => (_: ElementHandle<HTMLElement>) => Promise<Array<ElementHandle<Node>>>} */
export const _find = s => h => h.$$(s)

/** @type {(_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _click = h => h.click()

/** @type {(_: ElementHandle<HTMLElement>) => Promise<unknown>} */
export const _boundingBox = h => h.boundingBox()

/** @type {(_: ElementHandle<HTMLElement>) => Promise<unknown>} */
export const _boxModel = h => h.boxModel()

/** @type {(_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _focus = h => h.focus()

/** @type {(_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _hover = h => h.hover()

/** @type {(_: ElementHandle<HTMLElement>) => Promise<boolean>} */
export const _isHidden = h => h.isHidden()

/** @type {(_: ElementHandle<HTMLElement>) => Promise<boolean>} */
export const _isVisible = h => h.isVisible()

/** @type {(_: ElementHandle<HTMLElement>) => Promise<boolean>} */
export const _isIntersectingViewport = h => h.isIntersectingViewport()

/** @type {(_: ElementHandle<HTMLElement> | {x: number, y: number}) => (_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _drag = c => h => h.drag(c).then(() => {})

/** @type {(_: ElementHandle<HTMLElement>) => (_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _drop = from => to => to.drop(from)

/** @type {(_: import('puppeteer').ScreenshotOptions) => (_: ElementHandle<HTMLElement>) => Promise<Buffer>} */
export const _screenshot = o => h => h.screenshot(o).then(bs => Buffer.from(bs))

/** @type {(_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _scrollIntoView = h => h.scrollIntoView()

/** @type {(_: Array<string>) => (_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _select = o => h => h.select(...o).then(() => {})

/** @type {(_: ElementHandle<HTMLElement>) => Promise<void>} */
export const _tap = h => h.tap()

/** @type {(_: Array<string>) => (_: ElementHandle<HTMLInputElement>) => Promise<void>} */
export const _uploadFile = ps => h => h.uploadFile(...ps)

/** @type {(_: string) => (_: ElementHandle<HTMLElement>) => Promise<ElementHandle<Element> | null>} */
export const _waitForSelector = s => h => h.waitForSelector(s)

/** @type {(_: JSHandle<unknown>) => Promise<Array<{k: string, v: JSHandle<unknown>}>>} */
export const _getProperties = async h => {
  const ps = await h.getProperties()
  return Array.from(ps.entries()).map(([k, v]) => ({ k, v }))
}

/** @type {<T>(h: JSHandle<T>) => Promise<T>} */
export const _clone = h => h.jsonValue()
