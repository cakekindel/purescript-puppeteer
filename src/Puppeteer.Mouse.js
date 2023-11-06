/** @type {(_2: import('puppeteer').Mouse) => (_3: {x: number, y: number}) => (_1: {button: import('puppeteer').MouseButton, count: number, delay: number}) => () => Promise<void>} */
export const clickImpl =
  mouse =>
  ({ x, y }) =>
  opts =>
  () =>
    mouse.click(x, y, opts)

/** @type {(_2: import('puppeteer').Mouse) => (_3: import('puppeteer').MouseButton) => () => Promise<void>} */
export const downImpl = mouse => btn => () => mouse.down({ button: btn })

/** @type {(_2: import('puppeteer').Mouse) => (_3: import('puppeteer').MouseButton) => () => Promise<void>} */
export const upImpl = mouse => btn => () => mouse.up({ button: btn })

/** @type { (_2: import('puppeteer').Mouse) => (_3: {x: number, y: number}) => (_1: {steps: number}) =>() => Promise<void>} */
export const moveImpl =
  mouse =>
  ({ x, y }) =>
  opts =>
  () =>
    mouse.move(x, y, opts)

/** @type { (_2: import('puppeteer').Mouse) => (_3: {deltaX: number, deltaY: number}) => () => Promise<void>} */
export const scrollImpl = mouse => opts => () => mouse.wheel(opts)
