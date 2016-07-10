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

exports.anyButtonPressedP =
  function(conn, constant, just, nothing, maybe, fromNote, buttonPressState) {
    return function(){
      var out = constant(nothing);
      conn.input.on('message', function(deltaTime, message) {
        var isPressed = (parseInt(message[2],10) == 127)
        var ps = buttonPressState(isPressed);
        var button =
          maybe(false)(function(x) { return x })(fromNote(message[1]));

        console.log(deltaTime, message[1])
        if (!button || !isPressed) {
          out.set(nothing)
        } else {
          out.set(just({
            deltaTime: deltaTime,
            button: {
              ref: button,
              pressed: ps
            }
          }))
        }
      });
      return out;
    };
  };
