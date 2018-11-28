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
solc = solcWrapper(window.Module)



input = {
    // Required: Source code language, such as "Solidity", "Vyper", "lll", "assembly", etc.
    language: "Solidity",
    // Required
    sources:
    {
        // The keys here are the "global" names of the source files,
        // imports can use other files via remappings (see below).
        "foo.sol":
        {"content": `
            pragma solidity ^0.5.0;

            contract Foo {
                uint public num;
                event Log(
                    uint _value
                );
                constructor() public payable {
                    num = block.timestamp;
                    emit Log(num);
                }
                function deposit(uint _num) public payable returns(uint) {
                    emit Log(num);
                    uint old_num = num;
                    num = _num;
                    return old_num;
                }
            }`
        },
        "bar.sol":
        {"content": `
            pragma solidity ^0.5.0;

            contract Bar {
                uint public num;
                event Log(
                    uint _value
                );
                constructor() public payable {
                    num = block.timestamp;
                    emit Log(num);
                }
                function deposit(uint _num) public payable returns(uint) {
                    emit Log(num);
                    uint old_num = num;
                    num = _num;
                    return old_num;
                }
            }`
            //"pragma solidity ^0.5.0;contract Hell {    uint last_timestamp; constructor() public {        last_timestamp = block.timestamp;           }   }"}
        }
    },
    settings: {
        metadata: {
            // Use only literal content and not URLs (false by default)
            useLiteralContent: true
        },
        outputSelection: {
            // Enable the metadata and bytecode outputs of every single contract.
            "*": {
                "*": [ "metadata", "evm.bytecode", "abi" ]
            }
        }

    }
}


out = solc.compile(JSON.stringify(input), console.log)
code = JSON.parse(out)
contractBytecode = code.contracts["foo.sol"]['Foo'].evm.bytecode.object
contractABI = code.contracts["foo.sol"]['Foo'].abi
/*
 // create a new VM instance
 vm = new VM()


 console.log(code)

 vm.on('step', function (data) {
         console.log(data)
 })

 vm.runCode({
         code: Buffer.from(bytecode, 'hex'),
         gasLimit: Buffer.from('ffffffff', 'hex')
     }, function (err, results) {
         console.log(results)
         console.log('returned: ' + results.return.toString('hex'))
         console.log('gasUsed: ' + results.gasUsed.toString())
         console.log(err)
 })

 */

var sim = require('remix-simulator')
var Web3 = require('web3');
web3 = new Web3(new sim.Provider())

// web3.setProvider(new sim.Provider())
// eth = new Eth(new sim.Provider())
web3.eth.getAccounts().then((accounts) => {web3.eth.defaultAccount = accounts[0]

        contract1 = new web3.eth.Contract(contractABI)


        c = contract1.deploy({data: contractBytecode}).send({from: web3.eth.defaultAccount}).then(function(newContractInstance){
                console.log(newContractInstance.options.address) // instance with the new contract address
                contract1.options.address = newContractInstance.options.address


                contract1.methods.num().call().then(console.log)

                contract1.methods.deposit(123).send({from: web3.eth.defaultAccount}).on('receipt', function(receipt){
                        // receipt example
                        console.log(receipt)
                }).then(console.log)
        })

})




