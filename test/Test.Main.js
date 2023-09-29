/** @type {(_: Error) => () => string} */
export const errorString = e => () => e.toString()
