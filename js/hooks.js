function close_add_host() {
    $('#myModal').foundation('reveal','close');
}

function open_add_host() {
    $('#myModal').foundation('reveal','open');
}

function close_chart_modal() {
    $('#myChartModal').foundation('reveal','close');
}

function open_chart_modal() {
    $('#myChartModal').foundation('reveal','open');
}

function reinitialise_foundation(){
  $(document).foundation();
}

function start_joyride() {
  $(document).foundation('joyride', 'start');
}
