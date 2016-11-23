$(document).ready(function() {

	var rules = $('#rules');
	var p1v1 = $('#p1v1');
	var pvc = $('#pvc');
	var difficulties = $('#diff');
	var back_btn = $('#back_btn');

	difficulties.hide();
	back_btn.hide();

	pvc.click(function() {
		rules.hide();
		p1v1.hide();
		difficulties.show();
		back_btn.show();
	});

	back_btn.click(function() {
		rules.show();
		p1v1.show();
		difficulties.hide();
		back_btn.hide();
	});

});