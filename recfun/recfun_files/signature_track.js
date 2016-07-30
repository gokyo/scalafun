$(document).ready(function(){var e=site_base_url+"signature/course/"+spark_class_short_name+"/"+spark_class_id;jQuery.getJSON(spark_signature_url+"api/site/status",function(t){1==t.enabled&&jQuery.getJSON(spark_signature_url+"api/user/status",function(i){if(1===t.sign_up_now&&0===i.signature_track){if(1!=window.Cookie.get("spark_signaturetrack_hide")){var n="Earn a Verified Certificate for this course!",a="Get Started",r=window.ab.session.getExperiment("sigtrack_landing_topfold"),s=r.getChosenVariant();("title"===s||"title_threebox"===s)&&(n="Get official recognition for your coursework."),$("body").prepend("<div class='course-signaturetrack-topbanner'>  <span class='course-signaturetrack-topbanner-close'>    <a href='#'>&times;</a>  </span>  <span class='course-signaturetrack-topbanner-text'>"+n+"  </span>"+"  <span class='course-signaturetrack-topbanner-button'>"+"    <a target='_new' href='"+e+"?utm_source=spark&utm_medium=banner"+"'>"+a+"</a>"+"  </span>"+"</div>"),$(".course-signaturetrack-topbanner-close").click(function(){$(".course-signaturetrack-topbanner").hide(),window.Cookie.set("spark_signaturetrack_hide",1,{expires:new Date("2020"),path:spark_class_url})})}var o=function(e){return $('<div class="modal coursera-signature-modal-container">  <div class="coursera-signature-modal-header modal-header">    <button class="close" aria-hidden="true" data-modal-close>&times;</button>    <img class="coursera-signature-modal-ribbon" src="'+e.ribbon+'" />'+'    <p class="coursera-signature-modal-signature-track">Signature Track</p>'+'    <p class="coursera-signature-modal-title">'+e.timeLeft+" left to join!</p>"+"  </div>"+'  <div class="coursera-signature-modal-body modal-body" style="margin-left: 50px; margin-right: 50px;">'+"    <p>Hi "+e.studentName+",</p>"+"    <p></p>"+"    <p>We are really glad to see you in "+e.courseName+"! You've been invited to join the Signature Track, which allows you to earn a Verified Certificate for this course.</p>"+"    <p>Through this special option, you will be able to certify your success in this course by securely linking your coursework to your identity using your unique typing pattern and webcam.</p>"+'    <div style="text-align: center">'+'      <button class="btn coursera-signature-next-button course-signaturetrack-modal-learnmore" data-modal-close>'+"        Learn More"+"      </button>"+"    </div>"+'    <div class="course-signaturetrack-modal-nothanks">'+'      <a href="javascript:void(0)" data-modal-close>Not now</a>'+"    </div>"+'    <p style="font-size: 12px">Note: Joining the Signature Track is optional, you can still complete the course if you decide not to join.</p>'+"  </div>"+"</div>")};if(t.last_chance_dialog&&!i.last_chance_modal){var l=o({ribbon:site_static_asset_url+"/pages/signature/views/ribbon_stripes.png",timeLeft:t.duration_left,courseName:course_strings_name,studentName:student_full_name}),u=new Modal(l,{"overlay.class":"coursera-signature-modal-overlay-darker","overlay.close":!1});u.on("open",function(){$(".course-signaturetrack-modal-learnmore").on("click",function(){window.open(e+"?utm_source=spark&utm_medium=lastchance")})}),u.on("close",function(){$.get(spark_signature_url+"api/user/set_last_chance")}),u.open()}}else if(1===i.signature_track){var c=$('<div class="course-signaturetrack-status-modal-container"><h1>Signature Track Status</h1></div>');new Modal(c,{"overlay.class":"course-modal-overlay"})}})})});