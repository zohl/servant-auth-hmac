/// <reference path="element.d.ts"/>

import * as promise from './promise';
import {ajax, get, post} from './ajax';


interface App {
    init: (root: HTMLDivElement) => void;
}


let getFormByInput = (element: HTMLInputElement): HTMLFormElement => {
    let result: Node = element;

    while (result != null) {
        if (result instanceof HTMLFormElement) {
            return result;
        }
        result = result.parentNode;
    }
    return null;
};


let getFormData = (form: HTMLFormElement): any => {
    let result = {};

    let nodes = (<Element>form).querySelectorAll('input[type="text"], input[type="password"]');
    for (let i = 0; i < nodes.length; ++i) {
        let element = <HTMLInputElement>(nodes[i]);
        result[element.name] = element.value;
    }

    return result;
};



let app: App = (() => {

    let origin = document.location.origin;

    let domRoot: HTMLDivElement = null;

    let templates = {};
    let state = {};

    let render = (templateName: string) => {
      let result = templates[templateName];

      for(let key in state) {
        if(state.hasOwnProperty(key)) {
          result = result.replace('{{'+key+'}}', state[key].toString());
        }
      }
      return result;
    };


    let init = (root: HTMLDivElement) => {
        domRoot = root;

        domRoot.innerHTML = `
           <div class = "menu"></div>
           <hr/>
           <div class = "container"></div>
        `;

        let domMenu = <HTMLDivElement>domRoot.querySelector('.menu');
        let domContainer = <HTMLDivElement>domRoot.querySelector('.container');


        ajax(origin)(get('/api/templates')).then(xhr => {

            let menuHTML = '';
            let templateNames = <Array<string>>JSON.parse(xhr.response);
            templateNames.forEach(name => {
                menuHTML += ['<a href = "#', name, '">', name, '</a> '].join('');
            });
            domMenu.innerHTML = menuHTML;

            let anchors = domMenu.querySelectorAll('a');
            for (let i = 0; i < anchors.length; ++i) {
                let anchor = <HTMLAnchorElement>anchors[i];
                anchor.onclick = () => {
                    domContainer.innerHTML = render(anchor.hash.substr(1));
                    return false;
                };
            }

            promise.sequence(templateNames.map(name => ajax(origin)(get('/api/templates/' + name))))
                .then(xhrs => {
                    xhrs.forEach((xhr, i) => {
                        templates[templateNames[i]] = xhr.response;
                    });

                    if (anchors.length > 0) {
                        let anchor = <HTMLAnchorElement>anchors[0];
                        anchor.click();
                    }
                });
        });


        document.onclick = (event) => {
            if(event.target instanceof HTMLInputElement) {
                let element = event.target;
                if(element.type == 'submit') {
                    let form = getFormByInput(element);
                    if (form != null) {
                        ajax('')(post(form.action, getFormData(form)))
                            .then(xhr => {
                                state['token'] = JSON.parse(xhr.response);
                            })
                            .catch(err => {

                            });
                    }
                }
                return false;
            }
        };


        state = {
              'username': 'N/A'
            , 'token': 'N/A'
            , 'secret': 'N/A'
        };
    };

    return {
        init: init
    }
})();


window.onload = () => {
    let domApp = <HTMLDivElement>document.querySelector('.app');
    if (domApp != null) {
        app.init(domApp);
    }
};

