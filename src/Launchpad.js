// module Launchpad
"use strict"

var midi = require('midi');
exports.connectImpl = function(andThen){
  return function(config){
    return function(){
      var port = config.port
      console.log('a');
      var output = new midi.output();
      console.log('b');
      var input = new midi.input();
      console.log('c');
      output.openPort(port);
      console.log('d');
      input.openPort(port);
      console.log('e');
      input.on('message', function(deltaTime, message) {
        console.log('here' + message);
      });
      console.log('f');

      // Inexplicably requires an async break.
      setTimeout(function(){
        console.log('after');
        andThen({
          port: port,
          output: output,
          input: input
        });
      }, 0);
      console.log('h');
    };
  };
};

exports.disconnect = function(conn){
  return function(){
    //conn.output.closePort()
    //conn.input.closePort()
    return; // No-Op for now.
  };
};

exports.sendMessageImpl = function(conn, row, note, col){
  console.log('hi');
  return function(){
    conn.output.sendMessage([row, note, col])
  };
};
