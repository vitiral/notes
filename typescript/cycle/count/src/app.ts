import {VNode, div, button, p} from '@cycle/dom'
import {DOMSource} from '@cycle/dom/xstream-typings'
import xs, {Stream} from 'xstream'

export type Sources = {
  DOM: DOMSource
}

export type Sinks = {
  DOM: Stream<VNode>
}

// Some notes:
// A really funny thing about this is you have to kind
// of start at the bottom with just xs.of(DOM)
// and then start from the top again.
//
// Because of the cyclic nature, it isn't clear where you
// get DOM.select('.dec'), etc because you haven't yet
// declared the DOM!

export function App (sources: Sources): Sinks {
  // get a stream of actions expressed as +1 and -1
  const action$ = xs.merge(
    sources.DOM.select(".dec").events("click").mapTo(-1),
    sources.DOM.select(".inc").events("click").mapTo(1),
  );

  // convert this to a count stream by folding the stream
  // fold starts at a seed and calls (current, new)
  // on the specified function
  const count$ = action$.fold((current, next) => current + next, 0);

  // all that's left is displaying the dom. fold automatically
  // startWith's it's seed so the dom will be displayed.
  const vdom$ = count$.map(count =>
    div([
      button(".dec", "Decrement"),
      button(".inc", "increment"),
      p(`Counter: ${count}`),
    ])
  );

  const sinks = {
    DOM: vdom$
  }
  return sinks
}
