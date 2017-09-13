<template>
  <div class="tour-index"> 
    <router-view></router-view>
  </div>
</template>

<script>
export default {
  name: "tour-page",
  props: ["name", "tour"],
  data () {
    return {
    }
  }
}
</script>
