// Sort by the time the star was acquired in a certain day.
// temp1 is a private board JSON API result, like in https://adventofcode.com/2022/leaderboard/private/view/217019.json

let day = 6
let star = 1
Object.values(temp1.members)
  .map(i => [i.name, i.id, i.completion_day_level[day]])
  .filter(i => i[2])
  .filter(i => i[2][star])
  .sort((a,b) => a[2][star].get_star_ts - b[2][star].get_star_ts)
