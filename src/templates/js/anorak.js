addEvent(window, "load", init); 
var config, leagues, divisions, seasons;

function init()
{
  var req = new XMLHttpRequest();
  req.open("GET", "../../../selector.json");
  req.onreadystatechange = function() {if (req.readyState == 4) {config = eval('(' + req.responseText + ')'); configureSelector(); }};
  req.send(null);
}

function configureSelector()
{
  leagues = document.getElementById("leagues");
  divisions = document.getElementById("divisions");
  seasons = document.getElementById("seasons");
  for (var i = 0; i < config.leagues.length; i++)
  {
    var selected = leagueName == config.leagues[i].name;
    leagues.options[i] = new Option(config.leagues[i].name, null, selected, selected);
  }
  updateDivisions();
  addEvent(leagues, "change", function() {updateDivisions(); updateSeasons(); window.location.href = seasons.options[seasons.selectedIndex].value;});
  addEvent(divisions, "change", function() {updateSeasons(); window.location.href = seasons.options[seasons.selectedIndex].value;});
  addEvent(seasons, "change", function() {window.location.href = seasons.options[seasons.selectedIndex].value;});
}

function updateDivisions()
{
  var selectedDivs = config.leagues[leagues.selectedIndex].divisions;
  divisions.options.length = 0;
  for (var i = 0; i < selectedDivs.length; i++)
  {
    var selected = divisionName == selectedDivs[i].name;
    divisions.options[i] = new Option(selectedDivs[i].name, null, selected, selected);
  }
  updateSeasons();
}

function updateSeasons()
{
  var selectedSeasons = config.leagues[leagues.selectedIndex].divisions[divisions.selectedIndex].seasons;
  seasons.options.length = 0;
  for (var i = 0; i < selectedSeasons.length; i++)
  {
    var selected = seasonName == selectedSeasons[i].name;
    seasons.options[i] = new Option(selectedSeasons[i].name, selectedSeasons[i].link, selected, selected);
  }
}

function addEvent(obj, evType, fn)
{ 
  if (obj.addEventListener)
  { 
    obj.addEventListener(evType, fn, false); 
  }
  else if (obj.attachEvent)
  { 
    obj.attachEvent("on"+evType, fn); 
  }
}

