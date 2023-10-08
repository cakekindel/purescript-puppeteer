import { CDPSession } from 'puppeteer'

/** @typedef {import("puppeteer").ProtocolMapping.Commands} Commands */

/** @type {<T extends keyof Commands>(_: Commands[T]['paramsType'][0]) => (_: T) => (_: CDPSession) => () => Promise<Commands[T]['returnType']>} */
export const send = p => cmd => cdp => () => {
  // @ts-ignore
  return cdp.send(cmd, p)
}

/** @type {<T extends keyof Commands>(_: T) => (_: CDPSession) => () => Promise<Commands[T]['returnType']>} */
export const send0 = cmd => cdp => () => {
  // @ts-ignore
  return cdp.send(cmd)
}
