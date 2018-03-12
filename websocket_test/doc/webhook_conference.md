
# Conference webhooks

**Hooks**

* Create conference
    * hook_event: conference_create
* Destroy conference
    * hook_event: conference_destroy
* Add member to the conference
    * hook_event: conference_add_member
* Delete member from the conference
    * hook_event: conference_del_member
* Conference command
    * hook_event: conference_command
    * action modifiers:
    * conference_id modifier: <<"your_conference_id">>