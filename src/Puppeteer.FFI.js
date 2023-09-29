/** @type {() => Record<string, any>} */
const emptyRecord = () => ({})

/** @type {(_: Array<Record<string, any>>) => Record<string, any>} */
export const mergeRecords = rs =>
  rs.reduce((acc, cur) => {
    Object.assign(acc, cur)
    return acc
  }, emptyRecord())

/** @type {(m: Array<{k: string, v: any}>) => Record<string, any>} */
export const _mapToRecord = map =>
  map.reduce((r, {k, v}) => {
    r[k] = v
    return r
  }, emptyRecord())

/** @type {<T>(_: (_u: unknown) => T | null) => (_a: unknown) => T | undefined} */
export const _maybeToUndefined = mton => m => {
  const n = mton(m)
  return n === null ? undefined : n
}
