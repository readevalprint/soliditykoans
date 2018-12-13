/*
 * Example - Running code on an ethereum-vm
 *
 *
 * To run this example in the browser, use the pre-bundled
 * version (bundle.js) or bundle it with browserify
 * `browserify index.js -o bundle.js` and then load
 * this folder onto a HTTP WebServer (such as node-static)
 */
//var Buffer = require('safe-buffer').Buffer // use for Node.js <4.5.0
//var VM = require('../../index.js')
//var abi = require('ethereumjs-abi')
//rlp = require('rlp')


require('babel-polyfill')
const solcWrapper = require('solc/wrapper')
const sim = require('remix-simulator')
const Web3 = require('web3');
RemixTests = require('remix-tests');

solc = solcWrapper(window.Module)

web3 = new Web3(new sim.Provider())
