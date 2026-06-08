function setActiveLink(activeId) {
  document.querySelectorAll('.intro-link').forEach(link => {
    link.classList.remove('active-link');
  });
  document.getElementById(activeId).classList.add('active-link');
}

document.addEventListener("DOMContentLoaded", function() {
  document.getElementById('intro_link').addEventListener('click', function() {
    Shiny.setInputValue("introSection", "intro", { priority: "event" });
    setActiveLink('intro_link');
  });

  document.getElementById('stratefy_link').addEventListener('click', function() {
    Shiny.setInputValue("introSection", "strateFy", { priority: "event" });
    setActiveLink('stratefy_link');
  });

  document.getElementById('hodgson_link').addEventListener('click', function() {
    Shiny.setInputValue("introSection", "hodgson", { priority: "event" });
    setActiveLink('hodgson_link');
  });

  document.getElementById('morphophys_link').addEventListener('click', function() {
    Shiny.setInputValue("introSection", "morphoPhys", { priority: "event" });
    setActiveLink('morphophys_link');
  });
});

Shiny.addCustomMessageHandler("clearActiveSidebarLink", function(_) {
  document.querySelectorAll(".intro-link").forEach(link => {
    link.classList.remove("active-link");
  });
});
