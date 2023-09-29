export const _coord = c => c
export const _box = ([tl, tr, br, bl]) => ({ tl, tr, br, bl })
export const _boxModel = ({
  border,
  content,
  margin,
  padding,
  height,
  width,
}) => ({
  height,
  width,
  margin: _box(margin),
  padding: _box(padding),
  border: _box(border),
  content: _box(content),
})

export const _boundingBox = ({ height, width, x, y }) => ({
  height,
  width,
  coord: { x, y },
})
