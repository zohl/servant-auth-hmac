import * as promise from './promise';

type Signature = any;

let hmac = (key: string, message: string): Promise<string> => {

    let makeSignature = (cryptoKey: CryptoKey): Signature => {
        let algo = {name: "HMAC"};
        return window.crypto.subtle.sign(algo, cryptoKey, stringToArrayBuffer(message));
    };

    return promise.fmap(arrayBufferToString)(promise.bind(importKey(key), makeSignature));
};


let importKey = (key: string): Promise<CryptoKey> => {

  let algo = {
      name: "HMAC"
    , hash: {name: "SHA-256"}
    };

  return <Promise<CryptoKey>>window.crypto.subtle
        .importKey('raw', stringToArrayBuffer(key), algo, false, ["sign"]);
};


let stringToArrayBuffer = (s: string): Uint8Array => {
    let n = s.length;
    let result = new Uint8Array(n);

    let i;
    for (i = 0; i < n; ++i) {
        result[i] = s.charCodeAt(i);
    }
    return result;
};


let arrayBufferToString = (s: ArrayBuffer): string => String.fromCharCode(...new Uint8Array(s));


export {hmac};

