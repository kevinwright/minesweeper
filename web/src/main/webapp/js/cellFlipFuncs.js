function flip(id, newstate) {
  var cell = $('#'+id)
  cell.animate({
    height: 0
  }, 70, function() {
    cell.attr('class', newstate)
    cell.animate({
      height: 24
    }, 70, function() {
      //all done!
    });
  });
}

function cascadeGroup(ids, states) {
    if (ids.length > 0) {
        flip(ids.shift(), states.shift())
        cascadeGroup(ids, states)
    }
}

//seq = [{ids:[...],states:[...]}, {ids:[...],states:[...]}, ...]
function cascadeSeq(seq) {
    if (seq.length > 0) {
        var group = seq.shift()
        cascadeGroup(group.ids, group.states)
        setTimeout(function() {cascadeSeq(seq)},50);
    }
}