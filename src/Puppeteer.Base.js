/** @type {<T>(a: T) => T} */
export const unsafeLog = a => {
  console.log(a)
  return a
}

/** @type {<A extends object, B extends object>(a: A) => (b: B) => A & B} */
export const unsafeUnion = a => b => ({ ...a, ...b })
