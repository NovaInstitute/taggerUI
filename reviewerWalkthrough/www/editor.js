(function () {
  const open = new Set(['1']);
  let drag = null;
  document.addEventListener('toggle', e => {
    if (e.target.matches('.editor-branch')) {
      const id = e.target.dataset.branch;
      if (e.target.open) open.add(id); else open.delete(id);
    }
  }, true);
  function rename(row) {
    const button = row.querySelector('.editor-select');
    if (!button || row.querySelector('input')) return;
    const input = document.createElement('input');
    input.value = button.textContent;input.className='editor-inline';input.setAttribute('aria-label','Category name');
    button.hidden=true;button.after(input);input.focus();input.select();
    const finish = save => {
      if (!input.isConnected) return;
      if(save && input.value !== button.textContent) Shiny.setInputValue('editor_inline_rename',{id:row.dataset.node,label:input.value},{priority:'event'});
      button.hidden=false;input.remove();
    };
    input.addEventListener('keydown',e=>{e.stopPropagation();if(e.key==='Enter'){e.preventDefault();finish(true);}if(e.key==='Escape')finish(false);});
    input.addEventListener('click',e=>e.stopPropagation());
    input.addEventListener('blur',()=>finish(false));
  }
  document.addEventListener('click', e => {
    const button=e.target.closest('.editor-select,.editor-rename');
    if(!button)return;
    e.preventDefault();e.stopPropagation();
    if(button.matches('.editor-rename'))rename(button.closest('.editor-row'));
    else Shiny.setInputValue('editor_select',Number(button.dataset.node),{priority:'event'});
  });
  document.addEventListener('dragstart',e=>{
    const row=e.target.closest('.editor-row');if(!row)return;
    drag=row.dataset.node;e.dataTransfer.setData('text/plain',drag);e.dataTransfer.effectAllowed='move';
  });
  document.addEventListener('dragover',e=>{const row=e.target.closest('.editor-row');if(row&&drag){e.preventDefault();row.classList.add('drop-target');e.dataTransfer.dropEffect='move';}});
  document.addEventListener('dragleave',e=>{const row=e.target.closest('.editor-row');if(row)row.classList.remove('drop-target');});
  document.addEventListener('drop',e=>{
    const row=e.target.closest('.editor-row');if(!row||!drag)return;e.preventDefault();e.stopPropagation();
    row.classList.remove('drop-target');Shiny.setInputValue('editor_drop',{source:drag,destination:row.dataset.node},{priority:'event'});drag=null;
  });
  document.addEventListener('dragend',()=>{drag=null;document.querySelectorAll('.drop-target').forEach(x=>x.classList.remove('drop-target'));});
  document.addEventListener('shiny:connected',()=>{
    Shiny.addCustomMessageHandler('editorExpand',value=>document.querySelectorAll('.editor-branch').forEach(x=>{x.open=value||x.dataset.branch==='1';}));
    const tree=document.getElementById('editor_tree');
    if(tree)new MutationObserver(()=>tree.querySelectorAll('.editor-branch').forEach(x=>{if(open.has(x.dataset.branch))x.open=true;})).observe(tree,{childList:true,subtree:true});
  });
})();
