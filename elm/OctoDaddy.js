//  ,---.    ____      __       ___          __   __      ,---.
// ( @ @ )  / __ \____/ /____  / _ \___ ____/ /__/ /_ __ ( @ @ )
//  ).-.(  / /_/ / __/ __/ _ \/ // / _ `/ _  / _  / // /  ).-.(
// '/|||\` \____/\__/\__/\___/____/\_,_/\_,_/\_,_/\_, /  '/|||\`
//   '|`                                         /___/     '|`
//
// Adapted Calculator for my Daddy's Needs

var Elm = require( './src/Main.elm' ).Elm;
var app = Elm.Main.init({
    node: document.getElementById('elm')
});
