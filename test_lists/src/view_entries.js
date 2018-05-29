function(doc) {
  if (doc.pvt_deleted)
    return;
  if (doc.pvt_type === 'list_entry') {
    emit(doc.list_id, {
      capture_group_key: doc.capture_group_key,
      capture_group_length: doc.capture_group_length,
      name: doc.name,
      displayname: doc.displayname,
      firstname: doc.firstname,
      lastname: doc.lastname,
      number: doc.number,
      pattern: doc.pattern,
      prefix: doc.prefix,
      regexp: doc.regexp,
      type: doc.type
    })
  } else if (doc.pvt_type === 'list' && doc.entries) {
    for (var key in doc.entries) {
      if (!doc.entries.hasOwnProperty(key))
        continue;
      emit(doc._id, {
        capture_group_key: key,
        capture_group_length: doc.length,
        name: doc.entries[key].cid_name || doc.entries[key].name,
        number: doc.entries[key].cid_number || doc.entries[key].number,
        pattern: doc.entries[key].pattern,
        prefix: doc.entries[key].prefix,
        regexp: doc.entries[key].regexp,
        type: doc.entries[key].type
      })
    }
  } else {
    return
  }
}

function(doc) {
  if (doc.pvt_deleted)
    return;
  if (doc.pvt_type === 'list' && doc.entries) {
    emit(doc.list_id, doc);
  } else {
    return
  }
}


function(doc) {
  if (doc.pvt_deleted)
    return;
  emit(doc.list_id, doc);
}

//"map":
function(doc) {
    if (doc.pvt_type != 'user' || doc.pvt_deleted) return;

    var features = [];

    if(doc.smartpbx) {
        for(var feature in doc.smartpbx) {
            if(doc.smartpbx[feature].enabled) { features.push(feature); }
            }
    }

    if(doc.hotdesk && doc.hotdesk.enabled) { features.push('hotdesk');}

    if (doc.call_forward && doc.call_forward.enabled) {features.push('call_forward');}

    if (doc.call_forward && doc.call_forward.failover && !doc.call_forward.enabled) {features.push('call_forward_failover');}

    if (doc.caller_id && doc.caller_id.external && doc.caller_id.external.number) {features.push('caller_id');}

    if (doc.vm_to_email_enabled) {features.push('vm_to_email');}

    if (doc.music_on_hold && doc.music_on_hold.media_id) {features.push('music_on_hold');}

    if (doc.do_not_disturb && doc.do_not_disturb.enabled) {features.push('do_not_disturb');}
    }
    emit(doc.last_name + \" \" + doc.first_name, {'id': doc._id,'features': features,'username': doc.username,'email': doc.email,'first_name': doc.first_name,'last_name': doc.last_name,'priv_level': doc.priv_level,'feature_level': doc.feature_level, 'presence_id': doc.presence_id,'timezone': doc.timezone, 'call_recording': doc.call_recording});
}