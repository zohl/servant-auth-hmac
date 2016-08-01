import * as promise from './promise';

// BIG TODO

let hmac = (key: string, message: string): Promise<string> => {

    let makeSignature = key => {
        let algo = {name: "HMAC"};
        return window.crypto.subtle.sign(algo, key, base64ToArrayBuffer(data));
    };


    let processSignature = signature => {
        console.log(signature);
        return 'TODO';
    }

    return promise.fmap(processSignature)(promise.bind(importKey(key), makeSignature));
};


let importKey = (key: string): PromiseLike<CryptoKey> => {
  let algo = {
    name: "HMAC"
  , hash: {name: "SHA-256"}
  };

  return <Promise<CryptoKey>>window.crypto.subtle
        .importKey("raw", base64ToArrayBuffer(key), algo, false, ["sign"]);
};


let base64ToArrayBuffer = (s:string):ArrayBufferView => {
    let bs = window.atob(s);
    let n = bs.length;
    let result = new Uint8Array(n);

    let i;
    for (i = 0; i < n; ++i) {
        result[i] = bs.charCodeAt(i);
    }
    return result;
};


export {hmac};

