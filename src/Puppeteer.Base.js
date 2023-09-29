/** @type {<T>(a: T) => T} */
export const unsafeLog = a => {
  console.log(a)
  return a
}
