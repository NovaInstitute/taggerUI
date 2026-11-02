(function () {
  // Shiny replaces the contents of editor_tree whenever the draft changes.
  // Keep disclosure state outside that replaceable DOM so an edit does not
  // collapse the reviewer back to the root of the taxonomy.
  const openBranches = new Set(['1']);
  let drag = null;

  function rememberOpenBranches(tree) {
    if (!tree) return;
    tree.querySelectorAll('.editor-branch').forEach(branch => {
      const id = branch.dataset.branch;
      if (branch.open) openBranches.add(id); else openBranches.delete(id);
    });
    openBranches.add('1');
  }

  function restoreOpenBranches(tree) {
    if (!tree) return;
    tree.querySelectorAll('.editor-branch').forEach(branch => {
      branch.open = branch.dataset.branch === '1' || openBranches.has(branch.dataset.branch);
    });
  }

  document.addEventListener('toggle', e => {
    if (e.target.matches('.editor-branch')) {
      if (!e.target.isConnected) return;
      const id = e.target.dataset.branch;
      if (e.target.open) openBranches.add(id); else openBranches.delete(id);
      openBranches.add('1');
    }
  }, true);

  // Capture the current state while the old tree is still present. The
  // MutationObserver below restores it as soon as Shiny inserts the new tree.
  document.addEventListener('shiny:recalculating', e => {
    if (e.target && e.target.id === 'editor_tree') {
      rememberOpenBranches(e.target);
    }
  });
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
    const moveQuestion=e.target.closest('.editor-question-move');
    if(moveQuestion){
      e.preventDefault();e.stopPropagation();
      Shiny.setInputValue('editor_move_question',{id:moveQuestion.dataset.question,nonce:Date.now()},{priority:'event'});
      return;
    }
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
  let treeStateInitialised = false;
  function initialiseTreeState() {
    if (treeStateInitialised || !window.Shiny) return;
    const tree=document.getElementById('editor_tree');
    if(!tree)return;
    treeStateInitialised = true;
    Shiny.addCustomMessageHandler('editorExpand',value=>{
      openBranches.clear();
      tree.querySelectorAll('.editor-branch').forEach(branch=>{
        branch.open=value||branch.dataset.branch==='1';
        if(branch.open)openBranches.add(branch.dataset.branch);
      });
      openBranches.add('1');
    });
    restoreOpenBranches(tree);
    new MutationObserver(()=>restoreOpenBranches(tree)).observe(tree,{childList:true,subtree:true});
  }
  document.addEventListener('shiny:connected',initialiseTreeState);
  if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',initialiseTreeState);
  else initialiseTreeState();
})();
