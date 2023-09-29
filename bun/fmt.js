/** @type {(parser: string, ps: string[]) => import("bun").Subprocess} */
const prettier = (parser, ps) =>
  Bun.spawn(['bun', 'x', 'prettier', '--write', '--parser', parser, ...ps], {
    stdout: 'inherit',
    stderr: 'inherit',
  })

const procs = [
  prettier('babel', [
    './src/**/*.js',
    './bun/**/*.js',
    './.prettierrc.js',
    './tailwind/**/*.js',
  ]),
  prettier('json', ['./package.json', './jsconfig.json']),
  prettier('html', ['./assets/**/*.html']),
  prettier('css', ['./assets/**/*.css', './tailwind/**/*.css']),
  prettier('sh', ['./Dockerfile']),
  Bun.spawn(
    [
      'bun',
      'x',
      'purs-tidy',
      'format-in-place',
      'src/**/*.purs',
      'test/**/*.purs',
    ],
    {
      stdout: 'inherit',
      stderr: 'inherit',
    },
  ),
]

await Promise.all(procs.map(p => p.exited))
