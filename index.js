import { Elm } from './src/Main.elm'

Elm.Main.init({
  node: document.body,
})

// animate the title with decreasing frequency
const title = document.getElementById('title')
let animationThreshold = 0.2
const animationInterval = window.setInterval(() => {
  for (const c of title.children) {
    if (Math.random() > animationThreshold) {
      animateChar(c)
    }
  }
  animationThreshold += 0.01
  if (animationThreshold > 1) {
    window.clearInterval(animationInterval)
  }
}, 350)

function animateChar(c) {
  c.style.top = randPx(-1, 1)
  c.style.left = randPx(-1, 1)
  c.style.transform = `rotate(${randDeg(-3, 3)})`
}

function randPx(from, to) {
  return randUnit(from, to, 'px')
}

function randDeg(from, to) {
  return randUnit(from, to, 'deg')
}

function randUnit(from, to, unit) {
  const value = Math.round(from + Math.random() * (to - from))
  return `${value}${unit}`
}
