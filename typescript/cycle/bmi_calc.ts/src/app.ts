import {VNode, div, p, input, h2} from '@cycle/dom'
import {DOMSource} from '@cycle/dom/xstream-typings'
import xs, {Stream} from 'xstream'

export type Sources = {
  DOM: DOMSource
}

export type Sinks = {
  DOM: Stream<VNode>
}

// let's first have the sliders be their own components
//function render_weight_slider(weight: number): VNode {
//  return div([
//    `Weight ${weight} kg`,
//    input('.weight', {attrs: {type: 'range', min: 40, max: 140}}),
//  ]);
//}

//function render_height_slider(heightCm: number): VNode {
//  return div([
//    `Height ${heightCm} cm`,
//    input(".height", {attrs: {type: 'range', min: 100, max: 300}}),
//  ]);
//}

// and get some helper functions
function calc_bmi(heightCm:number, weightPounds:number): number {
      const heightMeters = heightCm * 0.01;
      return Math.round(weightPounds / (heightMeters * heightMeters));
}

//// the entire structure of the app should frequently be split into three
//// distinct parts: intent, model and view

//// the `view` is the simplest, and just involves taking the static state
//// at a moment in time and rendering it.

//class State {
//  constructor(
//    public weightPounds: number,
//    public heightCm: number,
//    public bmi: number) {};
//}

//function view(state$: Stream<State>): VNode {
//  return state$.map(({weightPounds, heightCm, bmi}) =>
//      div([
//        render_weight_slider(weightPounds),
//        render_height_slider(heightCm),
//        h2('BMI is ' + bmi),
//      ])
//    );
//}

//// model takes actions and converts them into the unrendered "state" of the
//// component

//function model(actions: Actions): Stream<State> {
//  const weight$ = actions.changeWeight$.startWith(70);
//  const height$ = actions.changeWeight$.startWith(170);

//  return xs.combine([weight$, height$])
//    .map(([weight, height]) => new Stream(
//      weight, height, calc_bmi(height, weight)))
//}


//// intent is the distilling of the user's *actions* as they pertain
//// to the app. It takes the signals from the user and creates actions

//interface Actions {
//  changeWeight$: Stream<number>,
//  changeHeight$: Stream<number>,
//}

//function intent(domSource: DOMSource): Actions {
//  // get the change events. `+` converts a str to a number (ick)
//  return {
//    changeWeight$: domSource.select('.weight').events('input')
//      .map(ev => +(<HTMLInputElement>ev.target).value),
//    changeHeight$: domSource.select('.height').events('input')
//      .map(ev => +(<HTMLInputElement>ev.target).value),
//  }
//}

export function App (sources: Sources): Sinks {
  const changeWeight$ = sources.DOM.select('.weight').events('input')
    .map(ev => +(<HTMLInputElement>ev.target).value);
  const changeHeight$ = sources.DOM.select('.height').events('input')
    .map(ev => +(<HTMLInputElement>ev.target).value);

  const weight$ = changeWeight$.startWith(70);
  const height$ = changeWeight$.startWith(170);

  const state$ = xs.combine(weight$, height$)
    .map(([weight, height]) =>
         [weight, height, calc_bmi(height, weight)]);

  const vdom$ = state$.map(([weightPounds, heightCm, bmi]) =>
      div([
        div([
          `Weight ${weightPounds} kg`,
          input('.weight', {attrs: {type: 'range', min: 40, max: 140}}),
        ]),
        div([
          `Height ${heightCm} cm`,
          input(".height", {attrs: {type: 'range', min: 100, max: 300}}),
        ]),
        h2('BMI is ' + bmi),
      ])
    );
  return {
    DOM: vdom$
  };
  //return {
  //  DOM: view(model(intent(sources))),
  //};
}
