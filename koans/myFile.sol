pragma solidity ^0.5.0;

contract Hell {
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
} 
