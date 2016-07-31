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

type AjaxCallback = (xhr: XMLHttpRequest) => void;


let ajax = (origin: string) =>
              promise.dispatch((params: AjaxParameters): Promise<XMLHttpRequest> => {
  let {method, uri, headers, data} = params;

  return new Promise((resolve: AjaxCallback, reject: AjaxCallback) => {

    let xhr = new XMLHttpRequest();

    xhr.onreadystatechange = () => {
      if(xhr.readyState == XMLHttpRequest.DONE) {
        if(xhr.status >= 200 && xhr.status < 300) {
          resolve(xhr);
        }
        else {
          reject(xhr);
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


let hmac = (key: string, message: string) => 'TODO';

let signed = (username: string, token: string) =>
    (params: AjaxParameters): Promise<AjaxParameters> => {

        let {method, uri, headers, data} = params;
        let timestamp = Math.floor(Date.now()/1000).toString();

        let hash = hmac(token, [
              username
            , (Date.now()/1000).toString()
            , uri
            , method
            , headers
                .map(h => h.name + h.value)
                .join('\n')
            , data
            ].join('\n'));

        let credentials = `HMAC hash="${hash}",id="${username}",timestamp="${timestamp}"`;

        return new Promise((response, reject) => {

            let newParams = {
                  method: method
                , uri: uri
                , headers: headers.concat({
                      name: 'Authorization'
                    , value: credentials
                })
                , data: data
            }
            response(newParams);
        });
};


export {ajax, get, post, put, del, signed};

