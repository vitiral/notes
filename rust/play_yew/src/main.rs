#![recursion_limit="512"]
#[macro_use]
extern crate stdweb;
#[macro_use]
extern crate yew;

use yew::prelude::*;
use yew::services::console::ConsoleService;

use stdweb::web::Date;

use stdweb::web::Node;
use stdweb::unstable::TryFrom;
use yew::virtual_dom::VNode;

struct Context {
    console: ConsoleService,
}

struct Model {
    value: i64,
}

enum Msg {
    Increment,
    Decrement,
    None,
    Bulk(Vec<Msg>),
}

impl Component<Context> for Model {
    type Msg = Msg;
    type Properties = ();

    fn create(_: Self::Properties, context: &mut Env<Context, Self>) -> Self {
        Model {
            value: 0,
        }
    }

    fn update(&mut self, msg: Self::Msg, context: &mut Env<Context, Self>) -> ShouldRender {
        match msg {
            Msg::Increment => {
                self.value = self.value + 1;
                context.console.log("plus one");
            }
            Msg::Decrement => {
                self.value = self.value - 1;
                context.console.log("minus one");
            }
            Msg::Bulk(list) => for msg in list {
                self.update(msg, context);
                context.console.log("Bulk action");
            },
            Msg::None => {
                context.console.log("No action");
                return false;
            }
        }
        true
    }
}


const SVG: &str = r#"
<svg width="400" height="110">
  <rect width="300" height="100" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" />
  Sorry, your browser does not support inline SVG.
</svg>
"#;

impl Renderable<Context, Model> for Model {
    fn view(&self) -> Html<Context, Self> {
        // html!{
        //     <h1>{"See inner html?:"}</h1>
        //     <div innerHtml=SVG,></div>
        //     <div>{ SVG }</div>
        // }

        let js_node = js! {
            var div = document.createElement("div");
            div.innerHTML = @{SVG.to_string()};
            console.log(div);
            return div;
        };
        eprintln!("js_node: {:?}", js_node);
        let node = Node::try_from(js_node).expect("convert js_node");
        VNode::VRef(node)

        // html! {
        //     <div>
        //         <p>{ "This is some SVG:" }</p>
        //         <div innerHTML=SVG,></div>
        //         <p></p>
        //         <p>
        //             <span>{ "Use the buttons below or go to "}</span>
        //             <a href="#increment",>{"#increment"}</a>
        //             <span>{" or "}</span>
        //             <a href="#decrement",>{"#decrement"}</a>
        //         </p>
        //         <nav class="menu",>
        //             <button onclick=|_| Msg::Increment,>{ "Increment" }</button>
        //             <button onclick=|_| Msg::Decrement,>{ "Decrement" }</button>
        //             <button onclick=|_| Msg::Bulk(vec![Msg::Increment, Msg::Increment]),>{ "Increment Twice" }</button>
        //         </nav>
        //         <p>{ self.value }</p>
        //         <p>{ Date::new().to_string() }</p>
        //     </div>
        // }
    }
}

fn main() {
    yew::initialize();

    let context = Context {
        console: ConsoleService,
    };

    let app: App<_, Model> = App::new(context);
    app.mount_to_body();
    yew::run_loop();
}
