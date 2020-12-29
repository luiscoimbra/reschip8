type rec boxStyle = {bg: option<string>, border: option<boxStyle>}
type rec t = {
  screen: (. unit) => t,
  fillRegion: (. ~color: string, ~chr: string, int, int, int, int) => unit,
  clearRegion: (int, int, int, int) => unit,
  render: (. unit) => unit,
}
and boxConfig = {
  parent: t,
  top: option<string>,
  left: option<string>,
  width: int,
  height: int,
  style: boxStyle,
}
@bs.val @bs.module external blessed: t = "blessed"

let screen = blessed.screen(.)
screen.render(.)

let init = () => ()

let draw = ui => {
  for y in 0 to 31 {
    for x in 0 to 63 {
      if ui[y][x] === 1 {
        screen.fillRegion(. ~color="5", ~chr=" ", x, x+ 2, y, y + 1)
      } else {
        screen.fillRegion(. ~color="2", ~chr=" ", x, x + 32, y, y + 1)
      }
    }
  }
  screen.render(.)
}
