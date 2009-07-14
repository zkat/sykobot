var username = 'bavardage';
var reponame = 'kiloseconds';
var api_url = 'http://github.com/api/v2/json/';


function get_blob(sha_id, path, callback) {
    url = api_url + 'blob/show/' + username + '/' + reponame + '/' + sha_id + '/' + path + '?callback=?';

    $.getJSON(url, 
	      function(data) {
		  callback(data.blob);
	      }
	      );
}

function get_branches(callback) {
    url = api_url + 'repos/show/' + username + '/' + reponame + '/branches?callback=?';
    $.getJSON(url,
	      function(data) {
		  callback(data.branches);
	      }
	      );
}

function get_master_sha_id(callback) {
    get_branches(function (branches) {
	    callback(branches['master']);
	});
}

function get_tree(sha_id, callback) {
    url = api_url + 'tree/show/' + username + '/' + reponame + '/' + sha_id + '?callback=?';

    $.getJSON(url,
	      function(data) {
		  callback(data.tree);
	      }
	      );
}

function debug(text) {
    //    $('#debug').append(text + '<br/>');
}

function show_implementations() {
    $('#implementations').addClass('loading');
    $('#implementations').html('<li>loading...</li>');

    function process_master_tree(treeitems) {
	$('#implementations').removeClass('loading');
	$('#implementations').html('');
	$.each(treeitems, function(i, item) {
		if(item.type=='tree') {
		    $('#implementations').append(
						 '<li id="' 
						 + item.sha 
						 + '"><span>' 
						 + item.name 
						 + '</span></li>'
						 );

		     $('#' + item.sha + ' span').click(function() {
			     expand_language(item.sha);
			 });
		}
	    });
    }

    function process_master_sha_id(sha_id) {
	get_tree(sha_id, process_master_tree);
    }

    get_master_sha_id(process_master_sha_id);
}

function hide_language(id) {
    $('#' + id + ' ul').remove();
    $('#' + id + ' span').unbind('click');
    $('#' + id + ' span').click(function() {
	    expand_language(id);
	});
}

function expand_language(id) {
    $('#' + id).append('<ul></ul>');
    $('#' + id + ' span').unbind('click')
    $('#' + id + ' span').click(function () {
	    hide_language(id);
	})
    $('#' + id + ' ul').addClass('loading');
    $('#' + id + ' ul').html('<li>loading...</li>');

    function process_language_tree(treeitems) {
	$('#' + id + ' ul').removeClass('loading');
	$('#' + id + ' ul').html('');
	$.each(treeitems, function(i, item) {
		if(item.type = 'blob') {
		    $('#' + id + ' ul').append(
		      '<li id="' + item.sha + '">'
		      + '<span onclick="show_implementation(\'' + id + '\', \'' + item.name + '\')">'
		      + item.name 
		      + '</span>'
		      + '</li>'
		    );
		}
	    });
    }

    get_tree(id, process_language_tree);
}

function show_implementation(tree_sha, path) {

    function parse_code(code){
	code=code.replace(/&/mg,'&#38;');
	code=code.replace(/</mg,'&#60;');
	code=code.replace(/>/mg,'&#62;');
	code=code.replace(/\"/mg,'&#34;');
	code=code.replace(/\t/g,'  ');
	code=code.replace(/\r?\n/g,'<br>');
	code=code.replace(/<br><br>/g,'<br>');
	code=code.replace(/ /g,'&nbsp;');
	return code;
    }

    function show_blob(blob) {

	raw_download_link = api_url + 'blob/show/' + username + '/' + reponame + '/' + blob.sha;
	bar = '<span><a href="' + raw_download_link + '">download</a> (save link as)</span>\n';
	text = '<pre><code>' + parse_code(blob.data) + '</code></pre>';
	show_popup(blob.name, bar, text);
    }
    
    get_blob(tree_sha, path, show_blob);
}

function show_popup(title, bar, text) {

    $('#popup').fadeOut('fast');
    $('#popup h1').html(title);
    $('#popupBar').html(bar);
    $('#popupText').html(text);

    var windowHeight = window.innerHeight; //document.documentElement.clientHeight;  
    var popupHeight = $('#popup').height();

    debug('height' + popupHeight);
    
    $("#popup").css({
	    'position': 'absolute',
		'top': windowHeight*0.1 + $(window).scrollTop(),  
		});
    $('#popup').fadeIn('fast');
}

function close_popup() {
    $('#popup').fadeOut('fast');
}

function get_recent_commits(callback) {
    function get_from_branch(branch) {
	url = api_url + 'commits/list/' 
	    + username + '/' + reponame + '/' 
	    + branch + '?callback=?';
	$.getJSON(url, 
		  function(data) {
		      callback(data.commits);
		  }
		  );
    }
    
    get_master_sha_id(get_from_branch);
}
	
	    
function show_recent_commits() {

    $('#recent-commits').html('<div class="commit">loading...</div>');
    $('#recent-commits').addClass('loading');
	

    function show_commits(commits) {
	$('#recent-commits').html('');
	$('#recent-commits').removeClass('loading');
	$.each(commits.slice(0,3), function(i, item) {
		text = '<div class="commit">';
		text += '<span class="commit-message">';
		text += item.message;
		text += '</span>';
		text += ' by ';
		text += item.author.name;
		text += '</div>';

		$('#recent-commits').append(text)
		    });
	$(".commit-message").truncate( 30, {
		trail: [ " ( <a href='#' class='truncate_show'>more</a> . . . )", " ( . . . <a href='#' class='truncate_hide'>less</a> )" ]
		    });

    }
    get_recent_commits(show_commits)
}

function update_clock() {
    ourDate = new Date();
    metricTime =  (ourDate.getHours() * 3600 + ourDate.getMinutes() * 60 + ourDate.getSeconds());
    $('#the-time').html((metricTime/1000).toFixed(3) + 'ks');

    setTimeout(update_clock, 500);
}

function truncate_more_info() {
    $('#more-info').truncate(10, {
	    trail: [ " ( <a href='#' class='truncate_show'>more</a> . . . )", " ( . . . <a href='#' class='truncate_hide'>less</a> )" ]
		});
}