// module Launchpad
"use strict"

var midi = require('midi');
exports.connect = function(config){
  return function(){
    var port = config.port
    var output = new midi.output();
    var input = new midi.input();
    output.openPort(port);
    input.openPort(port);
    input.on('message', function(deltaTime, message) {
      console.log('here' + message);
    });
    return {
      port: port,
      output: output,
      input: input
    };
  };
};

exports.disconnect = function(conn){
  return function(){
    conn.output.closePort()
    conn.input.closePort()
    return; // No-Op for now.
  };
};

exports.sendMessageImpl = function(conn, row, note, col){
  return function(){
    //console.log(row, note, col);
    conn.output.sendMessage([row, note, col])
  };
};
