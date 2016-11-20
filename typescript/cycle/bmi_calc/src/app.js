import {div, input} from '@cycle/dom'
import xs from 'xstream'

export function App (sources) {
  const vtree$ = xs.of(
    div([
      input('.height', {type: 'range'}),
    ])
  )
  const sinks = {
    DOM: vtree$
  }
  return sinks
}
