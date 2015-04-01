
type pool_t = {
  pool_uuid : string;
  pool_name_label : string;
  pool_name_description : string;
  pool_master : string;
  pool_default_SR : string;
}

type host_t = {
  

type objs =
  | Pool of pool_t
  | VM of vm_t
