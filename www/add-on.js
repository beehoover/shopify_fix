(function($) {
	$(document).ready(function(){
	
	/* 
	Open External Links in New Tabs
	========================================================
	*/
	$('a').filter(function() {
		return this.hostname && this.hostname !== location.hostname;})
		.attr("target","_blank")
		.before('<i class="fas fa-link"></i>')
		.css({'text-decoration': 'underline #1976d260'})
	});	
})(jQuery);

