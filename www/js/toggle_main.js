$( document ).ready(function() {
  setToggleMain()
  
  $(document).on('click','#' + consts.dom.sidebar_navigation_id + ' > .item', setToggleMain)
  
  $("#toggle_main_buttons_all").on("click", showToggleMainAll)
  
  $("#toggle_main_buttons_team").on("click", showToggleMainTeam)
})

function setToggleMain() {
  if(consts.main_toggle_all[$("#" + consts.dom.sidebar_navigation_id + " a.sidebar-active").attr("id")]) {
    console.log("ALL")
    showToggleMainAll()
  } else {
    console.log("TEAM")
    showToggleMainTeam()
  }
}

function showToggleMainAll() {
    $("#toggle_main_buttons > a")[1].classList.remove("active")
    $("#toggle_main_buttons > a")[0].classList.add("active")
    $("#main_team_wrapper").hide()
    $("#main_all_wrapper").show()
}

function showToggleMainTeam() {
    $("#toggle_main_buttons > a")[0].classList.remove("active")
    $("#toggle_main_buttons > a")[1].classList.add("active")
    $("#main_all_wrapper").hide()
    $("#main_team_wrapper").show()
}