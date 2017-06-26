import {VNode, makeDOMDriver, div, input, p} from '@cycle/dom';
import {DOMSource} from '@cycle/dom/xstream-typings';
import xs, {Stream} from 'xstream';

export type Sources = {
  DOM: DOMSource
}

export type Sinks = {
  DOM: Stream<VNode>
}

//interface CheckboxEventTarget extends EventTarget {
//  checked: boolean,
//}

export function App (sources: Sources): Sinks {
  const sinks = {
    DOM: sources.DOM.select('input').events('click')
      // getting the typing here is super hacky
      // I'm not sure how you are supposed to do this in typescript!
      // even worse, if you get the wrong attribute no good error messages
      // result. I guess testing is really important
      .map(ev => (<HTMLInputElement>ev.target).checked)
      // startWith sends a signal as soon as it is created
      // (it initializes the stream)
      .startWith(false)
      .map(toggled =>
        // this is where the actual rendered display is shown
        div([
          input({attrs: {type: 'checkbox', checked: toggled}}),
          'Toggle me',
          p(toggled ? 'ON' : 'off'),
        ])
      )
  };
  return sinks
}
