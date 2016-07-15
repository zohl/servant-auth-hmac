import * as promise from './promise';

type Method = 'GET' | 'POST' | 'PUT' | 'DELETE';

type Header = {
  name:  string;
  value: string;
}

type AjaxParameters = {
  method:  Method;
  uri:     string;
  headers: Header[];
  data?:   string;
};


type SuccessCallback = (xhr: XMLHttpRequest) => void;
type FailureCallback = (err: number) => void;


let ajax = (origin: string) =>
              promise.dispatch((params: AjaxParameters): Promise<XMLHttpRequest> => {
  let {method, uri, headers, data} = params;

  return new Promise((resolve: SuccessCallback, reject: FailureCallback) => {

    let xhr = new XMLHttpRequest();

    xhr.onreadystatechange = () => {
      if(xhr.readyState == XMLHttpRequest.DONE) {
        if(xhr.status >= 200 && xhr.status < 300) {
          resolve(xhr);
        }
        else {
          reject(xhr.status);
        }
      }
    };

    xhr.open(method, origin + uri, true);
    headers.forEach(h => xhr.setRequestHeader(h.name, h.value));
    xhr.send(data);
  });
});



let get = (uri: string): AjaxParameters => ({
    method: 'GET'
  , uri: uri
  , headers: []
  });

let post = (uri: string, data: any): AjaxParameters => ({
    method: 'POST'
  , uri: uri
  , headers: [{name: 'content-type', value: 'application/json'}]
  , data: JSON.stringify(data)
  });

let put = (uri: string, data: any): AjaxParameters => ({
    method: 'PUT'
  , uri: uri
  , headers: [{name: 'content-type', value: 'application/json'}]
  , data: JSON.stringify(data)
  });

let del = (uri: string): AjaxParameters => ({
    method: 'DELETE'
  , uri: uri
  , headers: []
  });


export {ajax, get, post, put, del};

