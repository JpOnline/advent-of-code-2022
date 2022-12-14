// Sort by the time the star was acquired in a certain day.
// temp1 is a private board JSON API result, like in https://adventofcode.com/2022/leaderboard/private/view/217019.json

let day = 14
let star = 2
let starTime = timestamp => (new Date(timestamp*1000)).toTimeString().substring(0,8)
Object.values(temp1.members)
  .map(i => [i.name, i.completion_day_level[day]])
  .filter(i => i[1])
  .filter(i => i[1][star])
  .sort((a,b) => a[1][star].get_star_ts - b[1][star].get_star_ts)
  .map(([name, stars]) => [starTime(stars[1]?.get_star_ts), starTime(stars[2]?.get_star_ts), name])
