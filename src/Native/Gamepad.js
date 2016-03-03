// Enable the API: about:config  dom.gamepad.enabled=true
// https://hacks.mozilla.org/2013/12/the-gamepad-api/
// http://luser.github.io/gamepadtest/
Elm.Native = Elm.Native || {};
Elm.Native.Gamepad = {};
Elm.Native.Gamepad.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Gamepad = localRuntime.Native.Gamepad || {};
  if (localRuntime.Native.Gamepad.values) {
    return localRuntime.Native.Gamepad.values;
  }

  var Signal = Elm.Signal.make(localRuntime),
      NS = Elm.Native.Signal.make(localRuntime),
      List = Elm.Native.List.make(localRuntime),
      gamepads = NS.input('gamepads', []),
      rAF = window.requestAnimationFrame ||
            window.mozRequestAnimationFrame ||
            window.webkitRequestAnimationFrame,
      getGamepads = navigator.getGamepads ||
                    navigator.webkitGetGamepads

  // JS GamepadButton -> Elm Gamepad.Button
  function button(b) {
    var pressed, value;
    if (typeof(b) == "object") {
      pressed = b.pressed; value = b.value;
    } else { // Old API
      pressed = (b == 1.0); value = b;
    }
    return {
      _: {},
      pressed: pressed,
      value: round(value)
    };
  }

  // JS Array Double -> Elm List Float
  function axes(xs) {
    return List.fromArray(xs.map(round));
  }

  // JS Array GamepadButton -> Elm List Gamepad.Button
  function buttons(xs) {
    return List.fromArray(xs.map(button));
  }

  // JS Gamepad -> Elm Gamepad.Gamepad
  function gamepad(t) {
    return {
      _: {},
      id: t.id,
      axes: axes(t.axes),
      buttons: buttons(t.buttons),
      mapping: t.mapping,
    };
  }

  // Chrome has a list of 4 empty gamepads
  function ensureArray(list) {
    if (list instanceof Array) {
      return list;
    } else {
      var x = [];
      for (var i=0; i<list.length; i++) {
        if (list[i]) {
          x.push(list[i]);
        }
      }
      return x;
    }
  }

  // Change events on gamepads are not supported by the spec[1]. So instead
  // we poll the controllers for updates, diff with the last state and emit a
  // Signal if anything has changed.
  //
  // [1]: http://www.w3.org/TR/gamepad/#other-events
  function updateGamepads(){
    var list = ensureArray(getGamepads.call(navigator)),
      current = gamepads.value,
      hasChanges = false;

    for (var i=0, l1=list.length, l2=current.length; i<l1 || i<l2; i++) {
      var left = current[i],
          right = list[i];
      if (!left) {
        hasChanges = true;
        current.push(gamepad(right));
      } else if (!right) {
        hasChanges = true;
        current.splice(i, 1);
      } else if (left.id != right.id) {
        hasChanges = true;
        current[i] = gamepad(right);
      } else {
        // TODO: check if updating objects in place is OK with Elm
        // TODO: Also update sub-items in place if OK to avoid GC
        if (!listArrayCmp(left.axes, right.axes, floatEq)) {
          hasChanges = true;
          left.axes = axes(right.axes);
        }
        if (!listArrayCmp(left.buttons, right.buttons, buttonEq)) {
          hasChanges = true;
          left.buttons = buttons(right.buttons);
        }
      }
    }

    if (hasChanges) {
      localRuntime.notify(gamepads.id, gamepads.value);
    }

    if (current.length > 0) {
      scheduleOne(updateGamepads);
    }
  }

  // Limit the noise, some controllers like to emit changes for really small
  // deltas. Is this enough precision ? YMMV
  function round(num) {
    return Math.round(num * 100) / 100;
  }

  // JS Double -> Elm Float -> Bool
  function floatEq(a, b) {
    return a == round(b);
  }

  // JS GamepadButton -> Elm Button -> Bool
  function buttonEq(a, b) {
    a = button(a);
    return a.pressed === b.pressed && floatEq(a.value, b.value);
  }

  // JS Array a -> Elm List b -> (a -> b -> Bool) -> Bool
  function listArrayCmp(list, array, isEq) {
    var pos = 0;
    while (list.ctor != "[]") {
      if (pos > array.length - 1) {
        return false;
      }
      if (!isEq(list._0, array[pos])) {
        return false;
      }
      pos += 1;
      list = list._1;
    }
    if (pos != array.length) {
      return false;
    }
    return true;
  }

  // Small utility function to make sure we only schedule a single
  // updateGamepads at the next requestAnimationFrame
  function scheduleOne(fn) {
    if (!fn.scheduled) {
      fn.scheduled = 0;
    }
    else if (fn.scheduled > 0) {
      return
    }
    fn.scheduled += 1;
    rAF(function() {
      fn.scheduled -= 1;
      fn();
    });
  }

  // Start polling with the updateGamepads function
  function poll() {
    scheduleOne(updateGamepads);
  }

  // Different scheduling options depending on the browser
  if ('GamepadEvent' in window) {
    // Only start polling after the first gamepad
    localRuntime.addListener([gamepads.id], window, 'gamepadconnected', poll);
  } else if (getGamepads) {
    // For older browsers
    setInterval(poll, 500);
  } else {
    // Not supported at all, the gamepads list will stay forever empty.
    console.log("Native/Gamepad: gamepads not supported");
  }

  return localRuntime.Native.Gamepad.values = {
    // FIXME: Not really sure what A2(Signal.map, ...) does
    gamepads: A2(Signal.map, List.fromArray, gamepads)
  };
};
