open Connections
open Lwt

let message (message_ref, message_rec) =
  let cls = match message_rec.API.message_cls with
  | `VM -> "VM"
  | `Host -> "Host"
  | `SR -> "SR"
  | `Pool -> "Pool"
  | `VMPP -> "VMPP" in
  let uuid = message_rec.API.message_uuid in
  let id = "drop-" ^ uuid in

  <:xml<
    <div class="row">
      <div class="medium-2 small-12 columns">
        $str:message_rec.API.message_timestamp$
      </div>
      <div class="medium-10 small-12 columns">
        <a data-dropdown="$str:id$" aria-controls="$str:id$" aria-expanded="false">
          $str:message_rec.API.message_name$
        </a>
        <div id="$str:id$" data-dropdown-content="" class="f-dropdown content" aria-hidden="true" tabindex="-1">
          <p>$str:message_rec.API.message_body$</p>
        </div>
      </div>
    </div>
  >>

let d = Dom_html.document
