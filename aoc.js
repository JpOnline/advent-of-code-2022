// Sort by the time the start was acquired in a certain day.
// temp1 is a private board JSON API result, like in https://adventofcode.com/2022/leaderboard/private/view/217019.json

let day = 6
let start = 1
Object.values(temp1)
  .map(i => [i.name, i.id, i.completion_day_level[day]])
  .filter(i => i[2])
  .sort((a,b) => a[2][start].get_star_ts - b[2][start].get_star_ts)
